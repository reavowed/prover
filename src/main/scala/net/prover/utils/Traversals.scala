package net.prover.utils

import monocle.{PTraversal, Traversal}
import scalaz.{Applicative, Traverse}
import scalaz.std.list.listInstance
import scalaz.syntax.traverse._

import scala.reflect.ClassTag

object Traversals {
  private implicit val seqTraverse: Traverse[Seq] = new Traverse[Seq] {
    override def traverseImpl[G[_], A, B](fa: Seq[A])(f: A => G[B])(implicit applicativeG: Applicative[G]) =
      applicativeG.map(listInstance.traverse(fa.toList)(f))(identity)
  }
  def filter[A](p: A => Boolean): Traversal[Seq[A], A] =
    new PTraversal[Seq[A], Seq[A], A, A] {
      def modifyF[F[_]: Applicative](f: A => F[A])(s: Seq[A]): F[Seq[A]] =
        s.traverse {
          case a if p(a) => f(a)
          case a => Applicative[F].point(a)
        }
    }
  def filterWithType[A, B <: A : ClassTag](p: B => Boolean) = new PTraversal[Seq[A], Seq[A], B, B] {
    override def modifyF[F[_]: Applicative](f: B => F[B])(s: Seq[A]) =
      s.traverse {
        case b
          if implicitly[ClassTag[B]].runtimeClass.isInstance(b) && p(b.asInstanceOf[B])
        =>
          Applicative[F].map(f(b.asInstanceOf[B]))(identity[A])
        case a => Applicative[F].point(a)
      }
  }
}
