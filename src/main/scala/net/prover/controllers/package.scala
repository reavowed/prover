package net.prover

import net.prover.exceptions.{BadRequestException, NotFoundException}
import org.springframework.http.{HttpStatus, ResponseEntity}

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
  }
}
