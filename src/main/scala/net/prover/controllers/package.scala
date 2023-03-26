package net.prover

import net.prover.model._
import net.prover.exceptions.{BadRequestException, NotFoundException}
import org.springframework.http.{HttpStatus, ResponseEntity}
import scalaz.{Functor, Monad}
import scalaz.syntax.functor._

import scala.util.{Failure, Success, Try}

package object controllers {
  implicit class TryToResponseEntityOps[T](t: Try[T]) {
    def toResponseEntity: ResponseEntity[_] = {
      t match {
        case Success(()) =>
          new ResponseEntity(HttpStatus.OK)
        case Success(value) =>
          new ResponseEntity[T](value, HttpStatus.OK)
        case Failure(NotFoundException(objectDescription)) =>
          new ResponseEntity(s"$objectDescription not found", HttpStatus.NOT_FOUND)
        case Failure(BadRequestException(message)) =>
          new ResponseEntity(message, HttpStatus.BAD_REQUEST)
        case Failure(e) =>
          throw e
      }
    }
    def orBadRequest(message: String): Try[T] = {
      t.recoverWith {
        case e => Failure(BadRequestException(s"$message: ${e.getMessage}"))
      }
    }
  }
  implicit class OptionWithResponseExceptionOps[T](option: Option[T]) {
    def orException(e: Exception): Try[T] = {
      option match {
        case Some(t) => Success(t)
        case None => Failure(e)
      }
    }
    def orNotFound(objectDescription: String): Try[T] = orException(NotFoundException(objectDescription))
    def orBadRequest(message: String): Try[T] = orException(BadRequestException(message))
    def orServerError(message: String): Try[T] = orException(new Exception(message))

    def badRequestIfDefined(getMessage: T => String): Try[Unit] = {
      option match {
        case Some(t) => Failure(BadRequestException(getMessage(t)))
        case None => Success(())
      }
    }
  }

  implicit class BooleanWithResponseExceptionOps(b: Boolean) {
    def orException(e: Exception): Try[Unit] = {
      if (b) Success(()) else Failure(e)
    }
    def orNotFound(objectDescription: String): Try[Unit] = orException(NotFoundException(objectDescription))
    def orBadRequest(message: String): Try[Unit] = orException(BadRequestException(message))
  }

  implicit class AnyWithResponseExceptionOps[T](t: => T) {
    def recoverWithBadRequest: Try[T] = recoverWithBadRequest(identity)
    def recoverWithBadRequest(f: String => String): Try[T] = {
      Try(t).recoverWith { case e => Failure(BadRequestException(f(e.getMessage)))}
    }
  }
}
