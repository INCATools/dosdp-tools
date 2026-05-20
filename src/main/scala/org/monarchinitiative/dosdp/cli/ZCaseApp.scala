package org.monarchinitiative.dosdp.cli

import caseapp.Name
import caseapp.core.{Error, RemainingArgs}
import caseapp.core.commandparser.CommandParser
import caseapp.core.help.{CommandsHelp, Help, WithHelp}
import caseapp.core.parser.Parser
import caseapp.core.util.Formatter
import zio.{Config => _, _}

import java.io.IOException

private object ZIOAppExitCodeHandling {

  def exitOnFailure(exitWith: ExitCode => UIO[Unit], exitCode: ExitCode): ZIO[Any, Nothing, ExitCode] =
    if (exitCode == ExitCode.success) ZIO.succeed(exitCode)
    else {
      // ZIOAppDefault derives the JVM status from effect success/failure, not a
      // returned ExitCode value. Keep command implementations free to return
      // ExitCode.failure, and translate that into a real process exit here.
      exitWith(exitCode).as(exitCode)
    }
}

/**
 * ZIO-flavored replacements for case-app's [[caseapp.CaseApp]] and
 * [[caseapp.CommandApp]] base classes.
 *
 * Why this exists: case-app's stock base classes assume a synchronous `def run`
 * returning `Unit`. They are not parameterized over an effect type, so any
 * subcommand that needs to thread `ZIO[Any, …, ExitCode]` through the entry
 * point would have to call `Runtime#unsafe.run` from inside a stock `run`. These
 * classes invert that: the abstract `run` returns ZIO, and the trampoline from
 * `args: List[String]` into `run` itself runs as a single ZIO program suitable
 * for a `zio.ZIOAppDefault` entry point.
 *
 * The structure mirrors `caseapp.cats.IOCaseApp` and
 * `caseapp.cats.CommandIOApp` from case-app-cats, but for `zio.ZIO`.
 *
 * The `WithHelp` branches in the pattern match cover the four possible parse
 * outcomes case-app produces: `--usage` requested, `--help` requested, a parse
 * error, or a successfully-parsed options value.
 */
abstract class ZCaseApp[T](implicit val parser0: Parser[T], val messages: Help[T]) extends ZIOAppDefault {

  private[this] def parser: Parser[T] = {
    val p = parser0.nameFormatter(nameFormatter)
    if (stopAtFirstUnrecognized)
      p.stopAtFirstUnrecognized
    else
      p
  }

  def run(options: T, remainingArgs: RemainingArgs): ZIO[Any, Nothing, ExitCode]

  private[this] def error(message: Error): ZIO[Any, IOException, ExitCode] =
    Console.printLine(message.message).as(ExitCode.failure)

  private[this] def helpAsked: ZIO[Any, IOException, ExitCode] =
    Console.printLine(messages.withHelp.help).as(ExitCode.success)

  private[this] def usageAsked: ZIO[Any, IOException, ExitCode] =
    Console.printLine(messages.withHelp.usage).as(ExitCode.success)

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

  private[this] def runArgs(args: List[String]): ZIO[Any, Nothing, ExitCode] =
    parser.withHelp.detailedParse(expandArgs(args), stopAtFirstUnrecognized) match {
      case Left(err)                                        => error(err).orDie
      case Right((WithHelp(_, true, _), _))                 => helpAsked.orDie
      case Right((WithHelp(true, _, _), _))                 => usageAsked.orDie
      case Right((WithHelp(_, _, Left(err)), _))            => error(err).orDie
      case Right((WithHelp(_, _, Right(t)), remainingArgs)) => run(t, remainingArgs)
    }

  override def run: ZIO[ZIOAppArgs, Nothing, ExitCode] =
    getArgs.flatMap(args => runArgs(args.toList).flatMap(ZIOAppExitCodeHandling.exitOnFailure(exit, _)))

}

abstract class ZCommandAppWithPreCommand[D, T](implicit
                                               val beforeCommandParser: Parser[D],
                                               baseBeforeCommandMessages: Help[D],
                                               val commandParser: CommandParser[T],
                                               val commandsMessages: CommandsHelp[T]
                                              ) extends ZIOAppDefault {

  /**
   * Override to support conditional early exit, suppressing a run.
   *
   * @param options       parsed options
   * @param remainingArgs extra arguments
   * @return exit code for early exit, none to call run
   */
  def beforeCommand(options: D, remainingArgs: Seq[String]): ZIO[Any, IOException, Option[ExitCode]]

  def run(options: T, remainingArgs: RemainingArgs): ZIO[Any, Nothing, ExitCode]

  def error(message: Error): ZIO[Any, IOException, ExitCode] =
    Console.printLine(message.message).as(ExitCode.failure)

  lazy val beforeCommandMessages: Help[D] =
    baseBeforeCommandMessages
      .withAppName(appName)
      .withAppVersion(appVersion)
      .withProgName(progName)
      .withOptionsDesc(s"[options] [command] [command-options]")
      .asInstanceOf[Help[D]] // circumventing data-class losing the type param :|

  lazy val commands: Seq[Seq[String]] = CommandsHelp[T].messages.map(_._1)

  def helpAsked(): ZIO[Any, IOException, ExitCode] =
    Console.printLine(
      s"""${beforeCommandMessages.help}
         |Available commands: ${commands.map(_.mkString(" ")).mkString(", ")}
         |
         |Type  $progName command --help  for help on an individual command"""
        .stripMargin)
      .as(ExitCode.success)

  def commandHelpAsked(command: Seq[String]): ZIO[Any, IOException, ExitCode] =
    Console.printLine(commandsMessages.messagesMap(command).helpMessage(beforeCommandMessages.progName, command))
      .as(ExitCode.success)

  def usageAsked(): ZIO[Any, IOException, ExitCode] =
    Console.printLine(
      s"""${beforeCommandMessages.usage}
         |Available commands: ${commands.map(_.mkString(" ")).mkString(", ")}
         |
         |Type  $progName command --usage  for usage of an individual command"""
        .stripMargin)
      .as(ExitCode.success)

  def commandUsageAsked(command: Seq[String]): ZIO[Any, IOException, ExitCode] =
    Console.printLine(commandsMessages.messagesMap(command).usageMessage(beforeCommandMessages.progName, command))
      .as(ExitCode.success)

  def appName: String = Help[D].appName

  def appVersion: String = Help[D].appVersion

  def progName: String = Help[D].progName

  private[this] def runArgs(args: List[String]): ZIO[Any, Nothing, ExitCode] = {
    if (args == List("--version")) ZIO.succeed(println(org.monarchinitiative.dosdp.cli.BuildInfo.toString)).as(ExitCode.success)
    else
      commandParser.withHelp.detailedParse(args.toVector)(beforeCommandParser.withHelp) match {
        case Left(err)                                                =>
          error(err).orDie
        case Right((WithHelp(true, _, _), _, _))                      =>
          usageAsked().orDie
        case Right((WithHelp(_, true, _), _, _))                      =>
          helpAsked().orDie
        case Right((WithHelp(false, false, Left(err)), _, _))         =>
          error(err).orDie
        case Right((WithHelp(false, false, Right(d)), dArgs, optCmd)) =>
          beforeCommand(d, dArgs).flatMap {
            case Some(exitCode) => ZIO.succeed(exitCode)
            case None           =>
              optCmd
                .map {
                  case Left(err)                                  =>
                    error(err).orDie
                  case Right((c, WithHelp(true, _, _), _))        =>
                    commandUsageAsked(c).orDie
                  case Right((c, WithHelp(_, true, _), _))        =>
                    commandHelpAsked(c).orDie
                  case Right((_, WithHelp(_, _, t), commandArgs)) =>
                    t.fold(
                      error(_).orDie,
                      run(_, commandArgs)
                    )
                }
                .getOrElse(ZIO.succeed(ExitCode.success))
          }.orDie
      }
  }

  override def run: ZIO[ZIOAppArgs, Nothing, ExitCode] =
    getArgs.flatMap(args => runArgs(args.toList).flatMap(ZIOAppExitCodeHandling.exitOnFailure(exit, _)))

}

abstract class ZCommandApp[T](implicit
                              commandParser: CommandParser[T],
                              commandsMessages: CommandsHelp[T]
                             ) extends ZCommandAppWithPreCommand[None.type, T] {

  override def beforeCommand(options: None.type, remainingArgs: Seq[String]): ZIO[Any, IOException, Option[ExitCode]] = {
    if (remainingArgs.nonEmpty) {
      error(Error.Other(s"Found extra arguments: ${remainingArgs.mkString(" ")}"))
        .map(Some(_))
    } else ZIO.none
  }
}
