package net.prover

import scala.collection.generic.CanBuildFrom
import scala.collection.{TraversableLike, mutable}
import scala.reflect.ClassTag

package object model {
  implicit class AnyOps[T](t: T) {
    def asOptionalInstanceOf[S : ClassTag]: Option[S] = {
      if (implicitly[ClassTag[S]].runtimeClass.isInstance(t)) {
        Some(t.asInstanceOf[S])
      } else {
        None
      }
    }
  }

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
    def reverse: (T, S) = (tuple._2, tuple._1)
  }

  implicit class SeqOps[T](seq: Seq[T]) {
    def mapFold[S](f: (T, Seq[S]) => S): Seq[S] = {
      seq.foldLeft(Seq.empty[S]) { case (acc, t) =>
        acc :+ f(t, acc)
      }
    }
    def mapAndFold[R, S](f: (T, Seq[S]) => (R, S)): (Seq[R], Seq[S]) = {
      seq.foldLeft((Seq.empty[R], Seq.empty[S])) { case ((rs, ss), t) =>
        val (r, s) = f(t, ss)
        (rs :+ r, ss :+ s)
      }
    }
    def collectFold[S](f: (Seq[S], T) => Option[S]): Option[Seq[S]] = {
      seq.foldLeft(Option(Seq.empty[S])) { case (accOption, t) =>
        accOption.flatMap { acc =>
          f(acc, t).map(acc :+ _)
        }
      }
    }
    def foldInAnyOrder[S](acc: S)(f: (S, T) => Option[S]): Option[S] = {
      def helper(left: Seq[T], failed: Seq[T], current: S): Option[S] = {
        left match {
          case head +: tail =>
            f(current, head) match {
              case Some(next) =>
                helper(failed ++ tail, Nil, next)
              case None =>
                helper(tail, failed :+ head, current)
            }
          case Nil =>
            failed match {
              case Nil =>
                Some(current)
              case _ =>
                None
            }
        }
      }
      helper(seq, Nil, acc)
    }
    def mapWithIndex[S](f: (T, Int) => S): Seq[S] = {
      seq.zipWithIndex.map { case (t, index) => f(t, index)}
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
    def toType[S: ClassTag]: Option[Seq[S]] = seq.map {
      case s: S =>
        Some(s)
      case _ =>
        None
    }.traverseOption
    def areAllOfType[S: ClassTag]: Boolean = seq.forall {
      case _: S =>
        true
      case _ =>
        false
    }
    def mapCollect[S](f: PartialFunction[T, Option[S]]): Seq[S] = {
      seq.map(f.lift).collect {
        case Some(Some(t)) => t
      }
    }
  }

  implicit class SeqTupleOps[S, T](seq: Seq[(S, T)]) {
    def split: (Seq[S], Seq[T]) = {
      (seq.map(_._1), seq.map(_._2))
    }
  }

  implicit class SetOps[T](set: Set[T]) {
    def ofType[S : ClassTag]: Set[S] = {
      set
        .collect {
          case s if implicitly[ClassTag[S]].runtimeClass.isInstance(s) => s.asInstanceOf[S]
        }
    }
  }

  implicit class TraversableOptionOps[T, Repr](traversable: TraversableLike[Option[T], Repr]) {
    def traverseOption[That](implicit bf: CanBuildFrom[Repr, T, That]): Option[That] = {
      traversable.foldLeft(Option(bf())) { case (builderOption, valueOption) =>
        for {
          builder <- builderOption
          value <- valueOption
        } yield builder += value
      }
    }.map(_.result())
  }

  implicit class SeqSetOps[T](seq: Seq[Set[T]]) {
    def knownCommonValues: Set[T] = {
      seq match {
        case Nil =>
          Set.empty
        case head +: tail =>
          tail.foldLeft(head)(_ intersect _)
      }
    }
  }

  implicit class IteratorOps[T](iterator: Iterator[T]) {
    def nextOption(): Option[T] = {
      if (iterator.hasNext)
        Some(iterator.next())
      else
        None
    }
    def mapCollect[S](f: T => Option[S]): Iterator[S] = {
      iterator.map(f).collect {
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
}
