package net.prover.util

import net.prover.model._
import scalaz.syntax.functor._
import scalaz.{Functor, Monad, Monoid}

import scala.util.{Success, Try}

object FunctorTypes {

  implicit val tryMonad: Monad[Try] = new Monad[Try] {
    override def point[A](a: => A): Try[A] = Success(a)
    override def bind[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa flatMap f
  }

  class WithValue[B] {
    type Type[A] = (A, B)
  }

  implicit def WithValueFunctor[B]: Functor[WithValue[B]#Type] = new Functor[WithValue[B]#Type] {
    override def map[A, C](input: (A, B))(f: A => C): (C, B) = input.mapLeft(f)
  }

  implicit def WithValueMonad[B : Monoid]: Monad[WithValue[B]#Type] = new Monad[WithValue[B]#Type] {
    override def point[A](a: => A): (A, B) = (a, Monoid[B].zero)
    override def bind[A, C](fa: (A, B))(f: A => (C, B)): (C, B) = f(fa._1).mapRight(Monoid[B].append(fa._2, _))
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

  implicit def FWithValueFunctor[F[_] : Functor, B]: Functor[FWithValue[F, B]#Type] = new Functor[FWithValue[F, B]#Type] {
    override def map[A, C](input: F[(A, B)])(f: A => C): F[(C, B)] = input.map(_.mapLeft(f))
  }

  class TryFWithValue[F[_], B] {
    type Type[A] = Try[F[(A, B)]]
  }

  implicit def TryFWithValueFunctor[F[_] : Functor, B]: Functor[TryFWithValue[F, B]#Type] = new Functor[TryFWithValue[F, B]#Type] {
    override def map[A, C](input: Try[F[(A, B)]])(f: A => C): Try[F[(C, B)]] = input.map(_.map(_.mapLeft(f)))
  }
}
