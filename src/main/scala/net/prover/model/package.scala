package net.prover

import scala.util.Try

package object model {
  implicit class StringOps(s: String) {
    def splitByWhitespace(max: Int = 0): Seq[String] = {
      s.trim.split("\\s+", max).toSeq
    }
    def splitKeyword = splitByWhitespace(2)
  }

  implicit class TupleOps[S,T](tuple: (S, T)) {
    def mapLeft[R](f: S => R): (R, T) = (f(tuple._1), tuple._2)
    def mapRight[R](f: T => R): (S, R) = (tuple._1, f(tuple._2))
  }

  implicit class SeqOps[T](x: Seq[T]) {
    def mapFold[S, U](initial: S)(f: (S, T) => (S, U)): Seq[U] = {
      x.foldLeft((initial, Seq.empty[U])) {
        case ((previous, list), element) =>
          f(previous, element).mapRight(list :+ _)
      }
    }._2
  }

  implicit class SeqOptionsOps[T](x: Seq[Option[T]]) {
    def traverseOption: Option[Seq[T]] = {
      x.foldLeft(Option(Seq.empty[T])) { case (sequenceOption, valueOption) =>
          for {
            sequence <- sequenceOption
            value <- valueOption
          } yield sequence :+ value
      }
    }
  }

  object SingleWord {
    def unapply(line: String): Option[(String, String)] = {
      line.splitByWhitespace(2) match {
        case Seq(word, restOfLine) =>
          Some((word, restOfLine))
        case Seq(word) =>
          Some((word, ""))
        case _ =>
          None
      }
    }
  }

  object IntParser {
    def unapply(s: String): Option[Int] = {
      Try(s.toInt).toOption
    }
  }
}
