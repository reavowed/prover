package net.prover.implicits

import net.prover._
import scalaz.{Functor, Monad}
import scalaz.syntax.functor._

import scala.util.{Success, Try}

trait IdentityTypeWorkaroundTrait {
  type Identity[+A] = A
}

trait FunctionalImplicits extends IdentityTypeWorkaroundTrait{
  implicit val identityMonad: Monad[Identity] = new Monad[Identity] {
    override def bind[A, B](a: A)(f: A => B): B = f(a)
    override def point[A](a: => A): A = a
  }
  // This technically violates the monad rules (for functions that catch exceptions), but we don't actually care
  implicit val tryMonad: Monad[Try] = new Monad[Try] {
    override def bind[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
    override def point[A](a: => A): Try[A] = Success(a)
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
