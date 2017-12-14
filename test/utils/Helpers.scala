package utils

import play.api.Logger

object Helpers {
  def time[R](msg: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    Logger.debug(msg + " executed in " + (t1 - t0) / 1000000 + "mS")
    result
  }
}
