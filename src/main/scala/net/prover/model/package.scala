package net.prover

import scala.collection.generic.CanBuildFrom
import scala.collection.{TraversableLike, mutable}
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

package object model {
  implicit class AnyOps[T](t: T) {
    def ifDefined[S](f: T => Option[S])(action: => Unit): T = {
      f(t).ifDefined(action)
      t
    }
    def ifEmpty[S](f: T => Option[S])(action: => Unit): T = {
      f(t).ifEmpty(action)
      t
    }
    def asOptionalInstanceOf[S : ClassTag]: Option[S] = {
      if (isRuntimeInstance[S]) {
        Some(t.asInstanceOf[S])
      } else {
        None
      }
    }
    def isRuntimeInstance[S : ClassTag]: Boolean = {
      implicitly[ClassTag[S]].runtimeClass.isInstance(t)
    }
  }

  implicit class StringOps(s: String) {
    def splitByWhitespace(max: Int = 0): Seq[String] = {
      s.trim.split("\\s+", max).toSeq.filter(_.nonEmpty)
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
    def single: Option[T]= {
      seq match {
        case Seq(singleElement) => Some(singleElement)
        case _ => None
      }
    }
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
    def flatMapWithIndex[S](f: (T, Int) => Seq[S]): Seq[S] = {
      seq.zipWithIndex.flatMap { case (t, index) => f(t, index)}
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
    def mapCollect[S](f: T => Option[S]): Seq[S] = {
      seq.map(f).collect {
        case Some(t) => t
      }
    }
    def zipStrict[S, That](other: Seq[S])(implicit bf: CanBuildFrom[Seq[T], (T, S), That]): Option[That] = {
      val b = bf(seq)
      val these = seq.iterator
      val those = other.iterator
      while (these.hasNext && those.hasNext)
        b += ((these.next(), those.next()))
      if (these.hasNext || those.hasNext)
        None
      else
       Some(b.result())
    }
    def mapFind[S](f: T => Option[S]): Option[S] = {
      seq.find { t => f(t).isDefined }.flatMap(f)
    }
    def findIndex(obj: T): Option[Int] = {
      val index = seq.indexOf(obj)
      if (index == -1)
        None
      else
        Some(index)
    }
    def findIndexWhere(f: T => Boolean): Option[Int] = {
      val index = seq.indexWhere(f)
      if (index == -1)
        None
      else
        Some(index)
    }
    def foldProduct[S](f: T => Seq[S]): Seq[Seq[S]] = {
      seq.foldLeft(Seq(Seq.empty[S])) { case (acc, t) =>
        for {
          ss <- acc
          s <- f(t)
        } yield ss :+ s
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
    def headOption: Option[T] = {
      if (iterator.hasNext)
        Some(iterator.next())
      else
        None
    }
    def findFirst[S](f: T => Option[S]): Option[S] = {
      iterator.map(f).collect { case Some(t) => t }.headOption
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

  implicit class OptionOps[T](x: Option[T]) {
    def ifDefined(action: => Unit): Option[T] = {
      if (x.nonEmpty) action
      x
    }
    def ifEmpty(action: => Unit): Option[T] = {
      if (x.isEmpty) action
      x
    }
  }
  implicit class TryOps[T](x: Try[T]) {
    def ifFailed(action: Throwable => Unit): Try[T] = {
      x match {
        case Failure(e) =>
          action(e)
        case _ =>
      }
      x
    }
  }

  implicit class SeqStringOps(seq: Seq[String]) {
    def indent: Seq[String] = seq.map("  " + _)
  }
}
