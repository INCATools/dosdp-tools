package org.monarchinitiative.dosdp.cli

import caseapp.Name
import caseapp.core.{Error, RemainingArgs}
import caseapp.core.commandparser.CommandParser
import caseapp.core.help.{CommandsHelp, Help, WithHelp}
import caseapp.core.parser.Parser
import caseapp.core.util.Formatter
import zio._
import zio.console.{Console, putStrLn}

/**
 * Adapted from caseapp.cats.IOCaseApp
 */
abstract class ZCaseApp[T](implicit val parser0: Parser[T], val messages: Help[T]) extends App {

  private[this] def parser: Parser[T] = {
    val p = parser0.nameFormatter(nameFormatter)
    if (stopAtFirstUnrecognized)
      p.stopAtFirstUnrecognized
    else
      p
  }

  def run(options: T, remainingArgs: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode]

  private[this] def error(message: Error): ZIO[Console, Nothing, ExitCode] =
    putStrLn(message.message).as(ExitCode.failure)

  private[this] def helpAsked: ZIO[Console, Nothing, ExitCode] =
    putStrLn(messages.withHelp.help).as(ExitCode.success)

  private[this] def usageAsked: ZIO[Console, Nothing, ExitCode] =
    putStrLn(messages.withHelp.usage).as(ExitCode.success)

  /**
   * Arguments are expanded then parsed. By default, argument expansion is the identity function.
   * Overriding this method allows plugging in an arbitrary argument expansion logic.
   *
   * One such expansion logic involves replacing each argument of the form '@<file>' with the
   * contents of that file where each line in the file becomes a distinct argument.
   * To enable this behavior, override this method as shown below.
   *
   * @example
   * {{{
   * import caseapp.core.parser.PlatformArgsExpander
   * override def expandArgs(args: List[String]): List[String]
   * = PlatformArgsExpander.expand(args)
   * }}}
   * @param args
   * @return
   */
  private[this] def expandArgs(args: List[String]): List[String] = args

  /**
   * Whether to stop parsing at the first unrecognized argument.
   *
   * That is, stop parsing at the first non option (not starting with "-"), or
   * the first unrecognized option. The unparsed arguments are put in the `args`
   * argument of `run`.
   */
  private[this] def stopAtFirstUnrecognized: Boolean = false

  private[this] def nameFormatter: Formatter[Name] = Formatter.DefaultNameFormatter

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    parser.withHelp.detailedParse(expandArgs(args), stopAtFirstUnrecognized) match {
      case Left(err)                                        => error(err)
      case Right((WithHelp(_, true, _), _))                 => helpAsked
      case Right((WithHelp(true, _, _), _))                 => usageAsked
      case Right((WithHelp(_, _, Left(err)), _))            => error(err)
      case Right((WithHelp(_, _, Right(t)), remainingArgs)) => run(t, remainingArgs)
    }

}

abstract class ZCommandAppWithPreCommand[D, T](implicit
                                               val beforeCommandParser: Parser[D],
                                               baseBeforeCommandMessages: Help[D],
                                               val commandParser: CommandParser[T],
                                               val commandsMessages: CommandsHelp[T]
                                              ) extends App {

  /**
   * Override to support conditional early exit, suppressing a run.
   *
   * @param options       parsed options
   * @param remainingArgs extra arguments
   * @return exit code for early exit, none to call run
   */
  def beforeCommand(options: D, remainingArgs: Seq[String]): ZIO[Console, Nothing, Option[ExitCode]]

  def run(options: T, remainingArgs: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode]

  def error(message: Error): ZIO[Console, Nothing, ExitCode] =
    putStrLn(message.message).as(ExitCode.failure)

  lazy val beforeCommandMessages: Help[D] =
    baseBeforeCommandMessages
      .withAppName(appName)
      .withAppVersion(appVersion)
      .withProgName(progName)
      .withOptionsDesc(s"[options] [command] [command-options]")
      .asInstanceOf[Help[D]] // circumventing data-class losing the type param :|

  lazy val commands: Seq[Seq[String]] = CommandsHelp[T].messages.map(_._1)

  def helpAsked(): ZIO[Console, Nothing, ExitCode] =
    putStrLn(
      s"""${beforeCommandMessages.help}
         |Available commands: ${commands.map(_.mkString(" ")).mkString(", ")}
         |
         |Type  $progName command --help  for help on an individual command"""
        .stripMargin)
      .as(ExitCode.success)

  def commandHelpAsked(command: Seq[String]): ZIO[Console, Nothing, ExitCode] =
    putStrLn(commandsMessages.messagesMap(command).helpMessage(beforeCommandMessages.progName, command))
      .as(ExitCode.success)

  def usageAsked(): ZIO[Console, Nothing, ExitCode] =
    putStrLn(
      s"""${beforeCommandMessages.usage}
         |Available commands: ${commands.map(_.mkString(" ")).mkString(", ")}
         |
         |Type  $progName command --usage  for usage of an individual command"""
        .stripMargin)
      .as(ExitCode.success)

  def commandUsageAsked(command: Seq[String]): ZIO[Console, Nothing, ExitCode] =
    putStrLn(commandsMessages.messagesMap(command).usageMessage(beforeCommandMessages.progName, command))
      .as(ExitCode.success)

  def appName: String = Help[D].appName

  def appVersion: String = Help[D].appVersion

  def progName: String = Help[D].progName

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    commandParser.withHelp.detailedParse(args.toVector)(beforeCommandParser.withHelp) match {
      case Left(err)                                                =>
        error(err)
      case Right((WithHelp(true, _, _), _, _))                      =>
        usageAsked()
      case Right((WithHelp(_, true, _), _, _))                      =>
        helpAsked()
      case Right((WithHelp(false, false, Left(err)), _, _))         =>
        error(err)
      case Right((WithHelp(false, false, Right(d)), dArgs, optCmd)) =>
        beforeCommand(d, dArgs).flatMap {
          case Some(exitCode) => IO.succeed(exitCode)
          case None           =>
            optCmd
              .map {
                case Left(err)                                  =>
                  error(err)
                case Right((c, WithHelp(true, _, _), _))        =>
                  commandUsageAsked(c)
                case Right((c, WithHelp(_, true, _), _))        =>
                  commandHelpAsked(c)
                case Right((_, WithHelp(_, _, t), commandArgs)) =>
                  t.fold(
                    error,
                    run(_, commandArgs)
                  )
              }
              .getOrElse(ZIO.succeed(ExitCode.success))
        }
    }

}

abstract class ZCommandApp[T](implicit
                              commandParser: CommandParser[T],
                              commandsMessages: CommandsHelp[T]
                             ) extends ZCommandAppWithPreCommand[None.type, T] {

  override def beforeCommand(options: None.type, remainingArgs: Seq[String]): ZIO[Console, Nothing, Option[ExitCode]] = {
    if (remainingArgs.nonEmpty) {
      error(Error.Other(s"Found extra arguments: ${remainingArgs.mkString(" ")}"))
        .map(Some(_))
    } else IO.none
  }
}