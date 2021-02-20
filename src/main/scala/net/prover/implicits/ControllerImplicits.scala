package net.prover.implicits

import net.prover._
import net.prover.exceptions.{BadRequestException, NotFoundException}
import org.springframework.http.{HttpStatus, ResponseEntity}
import scala.util.{Failure, Success, Try}

trait ControllerImplicits {
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
    def toEmptyResponseEntity: ResponseEntity[_] = {
      t.map(_ => ()).toResponseEntity
    }
    def orBadRequest(message: String): Try[T] = {
      t.recoverWith {
        case e => Failure(BadRequestException(s"$message: ${e.getMessage}"))
      }
    }
  }
  implicit class OptionWithResponseExceptionOps[T](option: Option[T]) {
    def orNotFound(objectDescription: String): Try[T] = option.orException(NotFoundException(objectDescription))
    def orBadRequest(message: String): Try[T] = option.orException(BadRequestException(message))
    def orServerError(message: String): Try[T] = option.orException(new Exception(message))

    def badRequestIfDefined(getMessage: T => String): Try[Unit] = {
      option match {
        case Some(t) => Failure(BadRequestException(getMessage(t)))
        case None => Success(())
      }
    }
  }

  implicit class BooleanWithResponseExceptionOps(b: Boolean) {
    def orNotFound(objectDescription: String): Try[Unit] = b.orException(NotFoundException(objectDescription))
    def orBadRequest(message: String): Try[Unit] = b.orException(BadRequestException(message))
  }

  implicit class AnyWithResponseExceptionOps[T](t: => T) {
    def recoverWithBadRequest: Try[T] = recoverWithBadRequest(identity)
    def recoverWithBadRequest(f: String => String): Try[T] = {
      Try(t).recoverWith { case e => Failure(BadRequestException(f(e.getMessage)))}
    }
  }
}
