package net.prover

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try

package object model {
  implicit class StringOps(s: String) {
    def splitByWhitespace(max: Int = 0): Seq[String] = {
      s.trim.split("\\s+", max).toSeq
    }
    def formatAsKey: String = splitByWhitespace().map(_.replaceAll("[\\W]+", "")).map(_.toLowerCase).mkString("-")
  }

  implicit class TupleOps[S,T](tuple: (S, T)) {
    def mapLeft[R](f: S => R): (R, T) = (f(tuple._1), tuple._2)
    def mapRight[R](f: T => R): (S, R) = (tuple._1, f(tuple._2))
    def mapBoth[U, R](f: S => U, g: T => R): (U, R) = (f(tuple._1), g(tuple._2))
    def optionMapLeft[R](f: S => Option[R]): Option[(R, T)] = f(tuple._1).map((_, tuple._2))
    def optionMapRight[R](f: T => Option[R]): Option[(S, R)] = f(tuple._2).map((tuple._1, _))
  }

  implicit class SeqOps[T](seq: Seq[T]) {
    def mapFold[S, U](initial: S)(f: (T, S) => (U, S)): (Seq[U], S) = {
      seq.foldLeft((Seq.empty[U], initial)) {
        case ((list, previous), element) =>
          f(element, previous).mapLeft(list :+ _)
      }
    }
    def distinctBy[S](f: T => S): Seq[T] = {
      val b = Seq.newBuilder[T]
      val seen = mutable.HashSet[S]()
      for (x <- seq) {
        val key = f(x)
        if (!seen(key)) {
          b += x
          seen += key
        }
      }
      b.result()
    }
    def ofType[S: ClassTag]: Seq[S] = seq.collect {
      case s: S =>
        s
    }
    def collectMany[S](f: PartialFunction[T, Seq[S]]): Seq[S] = {
      seq.collect(f).flatten
    }
    def mapCollect[S](f: T => Option[S]): Seq[S] = {
      seq.map(f).collectDefined
    }
  }

  implicit class SeqTupleOps[S, T](x: Seq[(S, T)]) {
    def split: (Seq[S], Seq[T]) = {
      (x.map(_._1), x.map(_._2))
    }
  }

  implicit class SeqOptionsOps[T](seq: Seq[Option[T]]) {
    def traverseOption: Option[Seq[T]] = {
      seq.foldLeft(Option(Seq.empty[T])) { case (sequenceOption, valueOption) =>
          for {
            sequence <- sequenceOption
            value <- valueOption
          } yield sequence :+ value
      }
    }

    def collectDefined: Seq[T] = {
      seq.collect {
        case Some(t) => t
      }
    }
  }

  implicit class IteratorOptionOps[T](iterator: Iterator[Option[T]]) {
    def collectDefined: Iterator[T] = {
      iterator.collect {
        case Some(t) => t
      }
    }
  }

  implicit class SeqParserOps[T](x: Seq[Parser[T]]) {
    def traverseParser: Parser[Seq[T]] = {
      x.foldLeft(Parser.constant(Seq.empty[T])) { case (seqParser, tParser) =>
        for {
          seq <- seqParser
          t <- tParser
        } yield {
          seq :+ t
        }
      }
    }
  }

  implicit class SeqParserTupleOps[S, T](x: Seq[(S, Parser[T])]) {
    def traverseParserMap: Parser[Map[S, T]] = {
      x.foldLeft(Parser.constant(Map.empty[S, T])) { case (mapParser, (s, tParser)) =>
        for {
          map <- mapParser
          t <- tParser
        } yield {
          map + (s -> t)
        }
      }
    }
  }

  object IntParser {
    def unapply(s: String): Option[Int] = {
      Try(s.toInt).toOption
    }
  }
}
