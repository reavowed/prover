package net.prover.util

import net.prover.model._
import scalaz.syntax.functor._
import scalaz.{Applicative, Functor, Monad, Monoid, Traverse}

import scala.util.{Success, Try}

object FunctorTypes {

  implicit val tryMonad: Monad[Try] with Traverse[Try] = new Monad[Try] with Traverse[Try] {
    override def point[A](a: => A): Try[A] = Success(a)
    override def bind[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa flatMap f
    override def traverseImpl[G[_] : Applicative, A, B](fa: Try[A])(f: A => G[B]): G[Try[B]] = fa match {
      case Success(a) => f(a).map(Success(_))
      case f: scala.util.Failure[A] => Applicative[G].point(f.asInstanceOf[scala.util.Failure[B]])
    }
  }

  class WithValue[B] {
    type Type[A] = (A, B)
  }

  implicit def WithValueTraverse[B]: Traverse[WithValue[B]#Type] = new Traverse[WithValue[B]#Type] {
    override def traverseImpl[G[_] : Applicative, A, C](fa: (A, B))(f: A => G[C]): G[(C, B)] = {
      f(fa._1).map(_ -> fa._2)
    }
  }

  implicit def WithValueMonad[B : Monoid]: Monad[WithValue[B]#Type] = new Monad[WithValue[B]#Type] {
    override def point[A](a: => A): (A, B) = (a, Monoid[B].zero)
    override def bind[A, C](fa: (A, B))(f: A => (C, B)): (C, B) = f(fa._1).mapRight(Monoid[B].append(fa._2, _))
  }

  class FWithValue[F[_], B] {
    type Type[A] = F[(A, B)]
  }

  implicit def FWithValueFunctor[F[_] : Functor, B]: Functor[FWithValue[F, B]#Type] = new Functor[FWithValue[F, B]#Type] {
    override def map[A, C](input: F[(A, B)])(f: A => C): F[(C, B)] = input.map(_.mapLeft(f))
  }

  implicit def FWithValueMonad[F[_] : Monad, B : Monoid]: Monad[FWithValue[F, B]#Type] = new Monad[FWithValue[F, B]#Type] {
    override def point[A](a: => A): F[(A, B)] = Monad[F].point((a, Monoid[B].zero))
    override def bind[A, C](fa: F[(A, B)])(f: A => F[(C, B)]): F[(C, B)] = {
      Monad[F].bind(fa) {
        case (a, b1) => f(a).map {
          case (c, b2) => (c, Monoid[B].append(b1, b2))
        }
      }
    }
  }

  implicit def FWithValueTraverse[F[_] : Traverse, B]: Traverse[FWithValue[F, B]#Type] = new Traverse[FWithValue[F, B]#Type] {
    override def traverseImpl[G[_] : Applicative, A, C](fa: F[(A, B)])(f: A => G[C]): G[F[(C, B)]] = {
      Traverse[F].traverseImpl(fa) {
        case (a, b) => Applicative[G].map(f(a))(_ -> b)
      }
    }
  }

  class TryF[F[_]] {
    type Type[A] = Try[F[A]]
  }

  implicit def TryFFunctor[F[_] : Functor]: Functor[TryF[F]#Type] = new Functor[TryF[F]#Type] {
    override def map[A, B](input: Try[F[A]])(f: A => B): Try[F[B]] = input.map(_.map(f))
  }

  class TryFWithValue[F[_], B] {
    type Type[A] = Try[F[(A, B)]]
  }

  implicit def TryFWithValueFunctor[F[_] : Functor, B]: Functor[TryFWithValue[F, B]#Type] = new Functor[TryFWithValue[F, B]#Type] {
    override def map[A, C](input: Try[F[(A, B)]])(f: A => C): Try[F[(C, B)]] = input.map(_.map(_.mapLeft(f)))
  }

  implicit def TryFWithValueTraverse[F[_] : Traverse, B]: Traverse[TryFWithValue[F, B]#Type] = new Traverse[TryFWithValue[F, B]#Type] {
    override def traverseImpl[G[_] : Applicative, A, C](tryFab: Try[F[(A, B)]])(f: A => G[C]): G[Try[F[(C, B)]]] = {
      Traverse[Try].traverseImpl(tryFab)(fab => FWithValueTraverse[F, B].traverseImpl(fab)(f))
    }
  }
}
