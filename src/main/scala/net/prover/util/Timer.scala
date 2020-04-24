package net.prover.util

import java.time.Duration

object Timer {
  def timeAction[T](description: String, logMethod: String => Unit)(calculation: => T): T = {
    val startTime = System.nanoTime()
    val result = calculation
    val stopTime = System.nanoTime()
    getFormattedDurationIfLarge(startTime, stopTime).foreach(d => logMethod(s"$description took $d"))
    result
  }

  private def getFormattedDurationIfLarge(startNanos: Long, endNanos: Long): Option[String] = {
    val duration = Duration.ofNanos(endNanos - startNanos)
    val millis = duration.toMillis
    if (millis == 0) {
      None
    } else if (millis < 1000) {
      Some(s"$millis ms")
    } else {
      val seconds = millis / 1000;
      val tenthsOfASecond = (millis % 1000) / 100
      Some(s"$seconds.$tenthsOfASecond s")
    }
  }
}
