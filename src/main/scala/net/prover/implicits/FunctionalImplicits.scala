package net.prover.implicits

import net.prover._
import scalaz.Functor
import scalaz.syntax.functor._

import scala.util.Try

trait FunctionalImplicits {
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
