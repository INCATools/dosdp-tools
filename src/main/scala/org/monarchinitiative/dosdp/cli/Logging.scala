package org.monarchinitiative.dosdp.cli

import zio._

trait Logging {

  def logDebug(message: => String): UIO[Unit] = ZIO.effectTotal(scribe.debug(message))

  def logDebug(message: => String, t: => Throwable): UIO[Unit] = ZIO.effectTotal(scribe.debug(message, t))

  def logInfo(message: => String): UIO[Unit] = ZIO.effectTotal(scribe.info(message))

  def logInfo(message: => String, t: => Throwable): UIO[Unit] = ZIO.effectTotal(scribe.info(message, t))

  def logWarn(message: => String): UIO[Unit] = ZIO.effectTotal(scribe.warn(message))

  def logWarn(message: => String, t: => Throwable): UIO[Unit] = ZIO.effectTotal(scribe.warn(message, t))

  def logError(message: => String): UIO[Unit] = ZIO.effectTotal(scribe.error(message))

  def logError(message: => String, t: => Throwable): UIO[Unit] = ZIO.effectTotal(scribe.error(message, t))

}
