package net.prover

import scala.util.Try

package object model {
  implicit class StringOps(s: String) {
    def splitByWhitespace(max: Int = 0): Seq[String] = {
      s.trim.split("\\s+", max).toSeq
    }
    def splitFirstWord: (String, String) = splitByWhitespace(2) match {
      case Seq(word, remainingText) =>
        (word, remainingText)
      case Seq(word) =>
        (word, "")
    }
    def formatAsKey: String = splitByWhitespace().map(_.replaceAll("[\\W]+", "")).map(_.toLowerCase).mkString("-")
  }

  object WordAndRemainingText {
    def unapply(line: PartialLine): Option[(String, PartialLine)] = {
      Some(line.splitFirstWord)
    }
    def unapply(line: BookLine): Option[(String, PartialLine)] = {
      Some(line.splitFirstWord)
    }
  }

  implicit class TupleOps[S,T](tuple: (S, T)) {
    def mapLeft[R](f: S => R): (R, T) = (f(tuple._1), tuple._2)
    def mapRight[R](f: T => R): (S, R) = (tuple._1, f(tuple._2))
    def optionMapLeft[R](f: S => Option[R]): Option[(R, T)] = f(tuple._1).map((_, tuple._2))
    def optionMapRight[R](f: T => Option[R]): Option[(S, R)] = f(tuple._2).map((tuple._1, _))
  }

  implicit class SeqOps[T](seq: Seq[T]) {
    def mapFold[S, U](initial: S)(f: (S, T) => (S, U)): (S, Seq[U]) = {
      seq.foldLeft((initial, Seq.empty[U])) {
        case ((previous, list), element) =>
          f(previous, element).mapRight(list :+ _)
      }
    }
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

  object IntParser {
    def unapply(s: String): Option[Int] = {
      Try(s.toInt).toOption
    }
  }
}
