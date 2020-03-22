package net.prover

import net.prover.model._
import net.prover.exceptions.{BadRequestException, NotFoundException}
import org.springframework.http.{HttpStatus, ResponseEntity}
import scalaz.Functor
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
    def orException(e: Exception): Try[T] = {
      option match {
        case Some(t) => Success(t)
        case None => Failure(e)
      }
    }
    def orNotFound(objectDescription: String): Try[T] = orException(NotFoundException(objectDescription))
    def orBadRequest(message: String): Try[T] = orException(BadRequestException(message))

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

  implicit val tryFunctor: Functor[Try] = new Functor[Try] {
    override def map[A,B](fa: Try[A])(f: A ⇒ B): Try[B] = fa map f
  }

  type Identity[A] = A
  implicit val identityFunctor: Functor[Identity] = new Functor[Identity] {
    override def map[A,B](a: Identity[A])(f: A ⇒ B): Identity[B] = f(a)
  }

  class WithValue[B] {
    type Type[A] = (A, B)
  }
  implicit def WithValueFunctor[B]: Functor[WithValue[B]#Type] = new Functor[WithValue[B]#Type] {
    override def map[A, C](input: (A, B))(f: A => C): (C, B) = input.mapLeft(f)
  }

  class TryWithValue[B] {
    type Type[A] = Try[(A, B)]
  }
  implicit def TryWithValueFunctor[B]: Functor[TryWithValue[B]#Type] = new Functor[TryWithValue[B]#Type] {
    override def map[A, C](input: Try[(A, B)])(f: A => C): Try[(C, B)] = input.map(_.mapLeft(f))
  }

  class FWithValue[F[_], B] {
    type Type[A] = F[(A, B)]
  }
  implicit def FWithValueFunctor[F[_]: Functor, B]: Functor[FWithValue[F, B]#Type] = new Functor[FWithValue[F, B]#Type] {
    override def map[A, C](input: F[(A, B)])(f: A => C): F[(C, B)] = input.map(_.mapLeft(f))
  }

  class TryFWithValue[F[_], B] {
    type Type[A] = Try[F[(A, B)]]
  }
  implicit def TryFWithValueFunctor[F[_]: Functor, B]: Functor[TryFWithValue[F, B]#Type] = new Functor[TryFWithValue[F, B]#Type] {
    override def map[A, C](input: Try[F[(A, B)]])(f: A => C): Try[F[(C, B)]] = input.map(_.map(_.mapLeft(f)))
  }
}
