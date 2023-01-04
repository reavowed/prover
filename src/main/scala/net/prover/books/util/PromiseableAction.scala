package net.prover.books.util

import scala.concurrent.{Future, Promise}
import scala.util.Try

case class PromiseableAction[-Input, +Output](action: Input => Output) {
  private[this] val promise: Promise[Output] = Promise()
  val future: Future[Output] = promise.future

  def execute(input: Input): Try[Output] = {
    val result = Try { action(input) }
    promise.complete(result)
    result
  }
}
