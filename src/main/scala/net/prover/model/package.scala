package net.prover

import java.nio.file.Path

import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import scala.collection.JavaConverters._
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
    def onlyIf(f: T => Boolean): Option[T] = {
      Some(t).filter(f)
    }
  }

  implicit class StringOps(s: String) {
    def splitByWhitespace(max: Int = 0): Seq[String] = {
      s.trim.split("\\s+", max).toSeq.filter(_.nonEmpty)
    }
    def capitalise: String = {
      s.headOption.map(_.toUpper).getOrElse("") + s.drop(1)
    }
    def camelCase: String = {
      val words = splitByWhitespace().map(_.replaceAll("[\\W]+", "")).map(_.toLowerCase)
      words.headOption.getOrElse("") + words.drop(1).map(_.capitalise).mkString("")
    }
    def formatAsKey: String = splitByWhitespace().map(_.replaceAll("[\\p{Punct}]+", "")).map(_.toLowerCase).mkString("-")
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
    def mapFold[R, S](initial: R)(f: (R, T) => (R, S)): (R, Seq[S]) = {
      seq.foldLeft((initial, Seq.empty[S])) { case ((acc, ss), t) =>
          f(acc, t).mapRight(ss :+ _)
      }
    }
    def mapFold[S](f: (Seq[S], T) => S): Seq[S] = {
      seq.foldLeft(Seq.empty[S]) { case (acc, t) =>
        acc :+ f(acc, t)
      }
    }
    def mapFoldOption[S](f: (Seq[S], T) => Option[S]): Option[Seq[S]] = {
      seq.foldLeft(Option(Seq.empty[S])) { case (accOption, t) =>
        accOption.flatMap { acc =>
          f(acc, t).map(acc :+ _)
        }
      }
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
      seq.iterator.map(f).find(_.isDefined).flatten
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
    def flatMapFoldProduct[S](init: S)(f: (S, T) => Seq[S]): Seq[S] = {
      seq.foldLeft(Seq(init)) { case (acc, t) =>
        acc.flatMap(f(_, t))
      }
    }
    def splitAtAll(f: T => Boolean): Seq[(Seq[T], T, Seq[T])] = {
      def helper(previous: Seq[T], next: Seq[T], acc: Seq[(Seq[T], T, Seq[T])]): Seq[(Seq[T], T, Seq[T])] = {
        next match {
          case t +: more =>
            if (f(t))
              helper(previous :+ t, more, acc :+ (previous, t, more))
            else
              helper(previous :+ t, more, acc)
          case Nil =>
            acc
        }
      }
      helper(Nil, seq, Nil)
    }
    def interleave[S >: T](s: S): Seq[S] = {
      seq match {
        case head +: tail =>
          head +: tail.flatMap(Seq(s, _))
        case _ =>
          seq
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
    def collectDefined[That](implicit bf: CanBuildFrom[Repr, T, That]): That = {
      traversable.collect {
        case Some(t) => t
      }
    }
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

  implicit class MapOps[S, T](map: Map[S, T]) {
    def tryAdd(key: S, value: T): Option[Map[S, T]] = {
      map.get(key) match {
        case Some(`value`) =>
          Some(map)
        case Some(_) =>
          None
        case None =>
          Some(map.updated(key, value))
      }
    }
    def merge(other: Map[S, T]): Option[Map[S, T]] = {
      other.foldLeft(Option(map)) { case (mapOptionSoFar, (key, value)) =>
        mapOptionSoFar.flatMap(_.tryAdd(key, value))
      }
    }
  }

  implicit class PathOps(path: Path) {
    def getAllChildFiles: Seq[Path] = {
      if (path.toFile.isDirectory)
        FileUtils.listFiles(path.toFile, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE).asScala
          .map(_.toPath)
          .toSeq
      else
        Nil
    }
  }
}
