package net.prover

import net.prover.util.PossibleSingleMatch
import net.prover.util.PossibleSingleMatch.{MultipleMatches, NoMatches, SingleMatch}

import scala.annotation.{tailrec, targetName}
import scala.collection.{View, immutable, mutable}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

package object model {
  extension [T] (t: T) {
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

  extension (s: String) {
    def splitByWhitespace(max: Int = 0): Seq[String] = {
      s.trim.split("\\s+", max).toSeq.filter(_.nonEmpty)
    }
    def splitByWhitespaceOrPunctuation(max: Int = 0): Seq[String] = {
      s.trim.split("[\\s-]+", max).toSeq.filter(_.nonEmpty)
    }
    def camelCase: String = {
      val words = splitByWhitespace().map(_.replaceAll("[\\W]+", "")).map(_.toLowerCase)
      words.headOption.getOrElse("") + words.drop(1).map(_.capitalize).mkString("")
    }
    def formatAsKey: String = splitByWhitespace().map(_.toLowerCase).mkString("-")
    def capitalizeWords: String = raw"\b((?<!\b')\w+)".r.replaceAllIn(s, _.group(1).capitalize)
    def inParens: String = "(" + s + ")"
    def decapitalize: String = s(0).toLower +: s.drop(1)
  }

  extension [S, T] (tuple: (S, T)) {
    def mapLeft[R](f: S => R): (R, T) = (f(tuple._1), tuple._2)
    def mapRight[R](f: T => R): (S, R) = (tuple._1, f(tuple._2))
    def optionMapLeft[R](f: S => Option[R]): Option[(R, T)] = f(tuple._1).map((_, tuple._2))
    def reverse: (T, S) = (tuple._2, tuple._1)
    def toSet(implicit f: S =:= T): Set[T] = Set(f(tuple._1), tuple._2)
  }

  extension [S, T, U] (tuple: (S, T, U)) {
    def map1[R](f: S => R): (R, T, U) = (f(tuple._1), tuple._2, tuple._3)
    def map2[R](f: T => R): (S, R, U) = (tuple._1, f(tuple._2), tuple._3)
    def map3[R](f: U => R): (S, T, R) = (tuple._1, tuple._2, f(tuple._3))
    def optionMap2[R](f: T => Option[R]): Option[(S, R, U)] = f(tuple._2).map((tuple._1, _, tuple._3))
    def strip3: (S, T) = (tuple._1, tuple._2)
  }

  extension [T1, T2, T3, T4] (tuple: (T1, T2, T3, T4)) {
    def optionMap1[S](f: T1 => Option[S]): Option[(S, T2, T3, T4)] = f(tuple._1).map((_, tuple._2, tuple._3, tuple._4))
  }

  extension [T] (seq: Seq[T]) {
    def headAndTailOption: Option[(T, Seq[T])] = +:.unapply(seq)
    def initAndLastOption: Option[(Seq[T], T)] = :+.unapply(seq)
    def flatMapFold[R, S](initial: R)(f: (R, T) => (R, Seq[S])): (R, Seq[S]) = {
      seq.foldLeft((initial, seq.iterableFactory.newBuilder[S])) { case ((acc, b), t) =>
        f(acc, t).mapRight(b ++= _)
      }.mapRight(_.result())
    }
    def mapReduceWithPrevious[S](f: (Seq[S], T) => S): Seq[S] = {
      seq.foldLeft(Seq.empty[S]) { case (acc, t) =>
        acc :+ f(acc, t)
      }
    }

    def findFirst[S](f: T => Option[S]): Option[S] = {
      seq.map(f).collectFirst { case Some(t) => t }
    }

    def mapFoldWithPrevious[R, S](initial: R)(f: (R, Seq[S], T) => (R, S)): (R, Seq[S]) = {
      seq.foldLeft((initial, Seq.empty[S])) { case ((acc, ss), t) =>
        f(acc, ss, t).mapRight(ss :+ _)
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
    def takeOfType[S: ClassTag]: (Seq[S], Seq[T]) = {

      @tailrec def helper(init: Seq[S], remaining: Seq[T]): (Seq[S], Seq[T]) = {
        remaining match {
          case (s: S) +: tail =>
            helper(init :+ s, tail)
          case _ =>
            (init, remaining)
        }
      }
      helper(Nil, seq)
    }
    def toType[S: ClassTag]: Option[Seq[S]] = seq.map {
      case s: S =>
        Some(s)
      case _ =>
        None
    }.traverseOption
    def mapCollect[S](f: T => Option[S]): Seq[S] = {
      seq.map(f).collect {
        case Some(t) => t
      }
    }
    def zipStrict[S, That](other: Seq[S]): Option[Seq[(T, S)]] = {
      val b = Seq.newBuilder[(T, S)]
      val these = seq.iterator
      val those = other.iterator
      while (these.hasNext && those.hasNext)
        b += ((these.next(), those.next()))
      if (these.hasNext || those.hasNext)
        None
      else
       Some(b.result())
    }
    def findIndexWhere(f: T => Boolean): Option[Int] = {
      val index = seq.indexWhere(f)
      if (index == -1)
        None
      else
        Some(index)
    }
    def findWithIndex(f: T => Boolean): Option[(T, Int)] = {
      seq.zipWithIndex.find(x => f(x._1))
    }
    def foldProduct[S](f: T => Seq[S]): Seq[Seq[S]] = {
      seq.foldLeft(Seq(Seq.empty[S])) { case (acc, t) =>
        for {
          ss <- acc
          s <- f(t)
        } yield ss :+ s
      }
    }
    def replaceValue(oldValue: T, newValue: T): Seq[T] = {
      seq.updated(seq.indexOf(oldValue), newValue)
    }
    def until(value: T): Seq[T] = {
      seq.takeWhile(_ != value)
    }
    def splitAtIndexIfValid(index: Int): Option[(Seq[T], T, Seq[T])] = {
      if (0 <= index && index < seq.length) {
        Some((seq.take(index), seq(index), seq.drop(index + 1)))
      } else {
        None
      }
    }
    def splitBetweenIndexesIfValid(firstIndex: Int, secondIndex: Int): Option[(Seq[T], Seq[T], Seq[T])] = {
      if (0 <= firstIndex && firstIndex <= secondIndex && secondIndex <= seq.length) {
        Some((seq.take(firstIndex), seq.slice(firstIndex, firstIndex + secondIndex - firstIndex), seq.drop(secondIndex)))
      } else {
        None
      }
    }
    def splitWhere(f: T => Boolean): Option[(Seq[T], T, Seq[T])] = {
      findIndexWhere(f).map { index =>
        (seq.take(index), seq(index), seq.drop(index + 1))
      }
    }
    def takeAndRemainingIfValid(index: Int): Option[(Seq[T], Seq[T])] = {
      if (0 <= index && index <= seq.length) {
        Some((seq.take(index), seq.drop(index)))
      } else {
        None
      }
    }

    def removeSingleValue(t: T): Option[Seq[T]] = {
      val index = seq.indexOf(t)
      if (index == -1)
        None
      else
        Some(seq.take(index) ++ seq.drop(index + 1))
    }

    def toMapWithKey[S](getKey: T => S): Map[S, T] = {
      seq.map(t => (getKey(t), t)).toMap
    }

    def toMapPreservingEarliest[S, U](implicit ev: T <:< (S, U)): Map[S, U] = {
      val b = Map.newBuilder[S, U]
      for (x <- seq)
        if (!b.result().contains(x._1)) b += x
      b.result()
    }

    def toSeqMap[S, U](implicit ev: T <:< (S, U)): Map[S, Seq[U]] = {
      val mapBuilder = Map.newBuilder[S, mutable.Builder[U, Seq[U]]]
      for (x <- seq) {
        if (!mapBuilder.result().contains(x._1)) {
          mapBuilder += (x._1 -> Seq.newBuilder[U])
        }
        mapBuilder.result()(x._1) += x._2
      }
      mapBuilder.result().view.mapValues(_.result()).toMap
    }
  }

  extension [T] (seq: Seq[Seq[T]]) {
    def minByLength: Option[Seq[T]] = seq match {
      case Nil => None
      case seq => Some(seq.minBy(_.length))
    }
  }

  extension [T, CC[_], C] (iterable: scala.collection.IterableOps[T, CC, C]) {
    def mapFold[R, S](initial: R)(f: (R, T) => (R, S)): (R, CC[S]) = {
      iterable.foldLeft((initial, iterable.iterableFactory.newBuilder[S])) { case ((accumulator, builder), t) =>
        f(accumulator, t).mapRight(builder += _)
      }.mapRight(_.result())
    }

    def mapFind[S](f: T => Option[S]): Option[S] = {
      iterable.iterator.map(f).find(_.isDefined).flatten
    }

    def single: Option[T] = {
      val iterator = iterable.iterator
      Some(())
        .filter(_ => iterator.hasNext)
        .map(_ => iterator.next())
        .filter(_ => !iterator.hasNext)
    }

    def dropUntil(p: T => Boolean): View[T] = {
      iterable.view.dropWhile(!p(_)).drop(1)
    }
  }

  extension [T, CC[_], C] (iterable: scala.collection.IterableOps[Option[T], CC, C]) {
    def collectDefined: CC[T] = {
      iterable.collect {
        case Some(t) => t
      }
    }

    def traverseOption: Option[CC[T]] = {
      iterable.foldLeft(Option(iterable.iterableFactory.newBuilder[T])) { case (builderOption, valueOption) =>
        for {
          builder <- builderOption
          value <- valueOption
        } yield builder += value
      }
    }.map(_.result())
  }

  extension [S, T] (seq: Seq[(S, T)]) {
    def split: (Seq[S], Seq[T]) = {
      (seq.map(_._1), seq.map(_._2))
    }
  }

  extension [S, T] (seq: Seq[(Seq[S], Seq[T])]) {
    def splitFlatten: (Seq[S], Seq[T]) = {
      (seq.flatMap(_._1), seq.flatMap(_._2))
    }
  }

  extension [S, T, R] (seq: Seq[(S, T, R)]) {
    def split: (Seq[S], Seq[T], Seq[R]) = {
      (seq.map(_._1), seq.map(_._2), seq.map(_._3))
    }
  }

  extension [T] (set: Set[T]) {
    def ofType[S : ClassTag]: Set[S] = {
      set
        .collect {
          case s if implicitly[ClassTag[S]].runtimeClass.isInstance(s) => s.asInstanceOf[S]
        }
    }
    def singleMatch: PossibleSingleMatch[T] = {
      if (set.isEmpty) {
        NoMatches
      } else {
        val iterator = set.iterator
        val firstMatch = iterator.next()
        if (iterator.hasNext) {
          MultipleMatches
        } else {
          SingleMatch(firstMatch)
        }
      }
    }
  }

  extension [T, CC[_], C] (traversable: scala.collection.IterableOps[Try[T], CC, C]) {
    def traverseTry: Try[CC[T]] = {
      traversable.foldLeft(Try(traversable.iterableFactory.newBuilder[T])) { case (builderTry, valueTry) =>
        for {
          builder <- builderTry
          value <- valueTry
        } yield builder += value
      }
    }.map(_.result())
  }

  extension [T] (iterator: Iterator[T]) {
    def headOption: Option[T] = {
      if (iterator.hasNext)
        Some(iterator.next())
      else
        None
    }
    def findFirst[S](f: T => Option[S]): Option[S] = {
      iterator.map(f).collect { case Some(t) => t }.headOption
    }
    def mapCollect[S](f: T => Option[S]): Iterator[S] = {
      iterator.map(f).collect {
        case Some(t) => t
      }
    }
    def collectOption[S](f: PartialFunction[T, Option[S]]): Iterator[S] = {
      iterator.collect(f).collect {
        case Some(t) => t
      }
    }
    def singleDistinctMatch: PossibleSingleMatch[T] = {
      @tailrec def hasDifferentMatch(i: Iterator[T], t: T): Boolean = {
        if (!i.hasNext)
          false
        else if (i.next() != t)
          true
        else
          hasDifferentMatch(i, t)
      }
      if (!iterator.hasNext) {
        NoMatches
      } else {
        val firstMatch = iterator.next()
        if (!hasDifferentMatch(iterator, firstMatch)) {
          SingleMatch(firstMatch)
        } else {
          MultipleMatches
        }
      }
    }
  }

  extension (x: Boolean) {
    def orFail(e: => Exception): Try[Unit] = if (x) {
      Success(())
    } else {
      Failure(e)
    }
  }

  extension [T] (x: Option[T]) {
    def ifDefined(action: => Unit): Option[T] = {
      if (x.nonEmpty) action
      x
    }
    def ifEmpty(action: => Unit): Option[T] = {
      if (x.isEmpty) action
      x
    }
    def failIfUndefined(e: => Exception): Try[T] = x match {
      case Some(t) => Success(t)
      case None => Failure(e)
    }
  }

  extension [T] (x: Option[Option[T]]) {
    def swap: Option[Option[T]] = x match {
      case Some(None) => None
      case None => Some(None)
      case _ => x
    }
  }

  extension [T] (x: Option[Try[T]]) {
    def orException(exception: Exception): Try[T] = x match {
      case Some(t) => t
      case None => Failure(exception)
    }
    def mapMap[S](f: T => S): Option[Try[S]] = x.map(_.map(f))
    def swap: Try[Option[T]] = x match {
      case Some(tryT) => tryT.map(Some(_))
      case None => Success(None)
    }
  }

  extension (seq: Seq[String]) {
    def indent: Seq[String] = seq.map("  " + _)
    def indentInLabelledBracesIfPresent(label: String): Seq[String] = {
      if (seq.nonEmpty) {
        (label + " {") +: seq.indent :+ "}"
      } else {
        Nil
      }
    }
    def optionalListInParens(separator: String): Option[String] = {
      Some(seq).filter(_.nonEmpty).map(_.mkString(separator).inParens)
    }
  }

  extension [S, T] (map: Map[S, T]) {
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
    def replace(key: S, f: T => T): Map[S, T] = {
      map.get(key).map(t => map.updated(key, f(t))) getOrElse map
    }
  }

  extension [S, T] (map: Map[S, Option[T]]) {
    def traverseOption: Option[Map[S, T]] = {
      map.map { case (s, tOption) => tOption.map(s -> _) }
        .traverseOption
        .map(_.toMap)
    }
  }

  extension [A, B] (seq: Seq[Either[A, B]]) {
    @targetName("splitEither")
    def split: (Seq[A], Seq[B]) = {
      val aBuilder = Seq.newBuilder[A]
      val bBuilder = Seq.newBuilder[B]
      for (x <- seq) {
        x match {
          case Left(a) =>
            aBuilder += a
          case Right(b) =>
            bBuilder += b
        }
      }
      (aBuilder.result(), bBuilder.result())
    }
  }
}
