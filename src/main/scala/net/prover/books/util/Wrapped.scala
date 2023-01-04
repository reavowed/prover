package net.prover.books.util

import scalaz.syntax.functor._

import scala.language.higherKinds

trait Wrapped[T] {
  def map[S](f: T => S): Wrapped[S]
  def foreach(f: T => Unit): Unit
}

object Wrapped {
  case class Functor[F[_] : scalaz.Functor, T](value: F[T]) extends Wrapped[T] {
    override def map[S](f: T => S): Wrapped[S] = Functor(value.map(f))
    override def foreach(f: T => Unit): Unit = value.map(f)
  }

  def apply[F[_] : scalaz.Functor, T](value: F[T]): Functor[F, T] = Functor(value)
}
