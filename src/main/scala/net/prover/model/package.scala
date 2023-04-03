package net.prover

import net.prover.util.PossibleSingleMatch
import net.prover.util.PossibleSingleMatch.{MultipleMatches, NoMatches, SingleMatch}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.{AbstractIterator, Iterator, TraversableLike, immutable, mutable}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

package object model {
  implicit class AnyOps[T](t: T) {
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
  }

  implicit class Tuple2Ops[S,T](tuple: (S, T)) {
    def mapLeft[R](f: S => R): (R, T) = (f(tuple._1), tuple._2)
    def mapRight[R](f: T => R): (S, R) = (tuple._1, f(tuple._2))
    def optionMapLeft[R](f: S => Option[R]): Option[(R, T)] = f(tuple._1).map((_, tuple._2))
    def reverse: (T, S) = (tuple._2, tuple._1)
    def toSet(implicit f: S =:= T): Set[T] = Set(f(tuple._1), tuple._2)
  }

  implicit class Tuple3Ops[S,T,U](tuple: (S, T, U)) {
    def map1[R](f: S => R): (R, T, U) = (f(tuple._1), tuple._2, tuple._3)
    def map2[R](f: T => R): (S, R, U) = (tuple._1, f(tuple._2), tuple._3)
    def map3[R](f: U => R): (S, T, R) = (tuple._1, tuple._2, f(tuple._3))
    def optionMap2[R](f: T => Option[R]): Option[(S, R, U)] = f(tuple._2).map((tuple._1, _, tuple._3))
    def strip3: (S, T) = (tuple._1, tuple._2)
  }
  implicit class Tuple4Ops[T1, T2, T3, T4](tuple: (T1, T2, T3, T4)) {
    def optionMap1[S](f: T1 => Option[S]): Option[(S, T2, T3, T4)] = f(tuple._1).map((_, tuple._2, tuple._3, tuple._4))
  }

  implicit class SeqOps[T](seq: Seq[T]) {
    def headAndTailOption: Option[(T, Seq[T])] = +:.unapply(seq)
    def initAndLastOption: Option[(Seq[T], T)] = :+.unapply(seq)
    def singleMatch: PossibleSingleMatch[T]= {
      seq match {
        case Nil => NoMatches
        case Seq(singleElement) => SingleMatch(singleElement)
        case _ => MultipleMatches
      }
    }
    def flatMapFold[R, S](initial: R)(f: (R, T) => (R, Seq[S])): (R, Seq[S]) = {
      seq.foldLeft((initial, Seq.empty[S])) { case ((acc, ss), t) =>
        f(acc, t).mapRight(ss ++ _)
      }
    }
    def mapReduceWithPrevious[S](f: (Seq[S], T) => S): Seq[S] = {
      seq.foldLeft(Seq.empty[S]) { case (acc, t) =>
        acc :+ f(acc, t)
      }
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

    def dropUntil(p: T => Boolean): Seq[T] = {
      seq.dropWhile(t => !p(t)).drop(1)
    }

    def toMapWithKey[S](getKey: T => S): Map[S, T] = {
      seq.map(t => (getKey(t), t)).toMap
    }
  }

  implicit class SeqSeqOps[T](seq: Seq[Seq[T]]) {
    def minByLength: Option[Seq[T]] = seq match {
      case Nil => None
      case seq => Some(seq.minBy(_.length))
    }
  }

  implicit class IterableOps[T](iterable: Iterable[T]) {
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
  }

  implicit class Seq2TupleOps[S, T](seq: Seq[(S, T)]) {
    def split: (Seq[S], Seq[T]) = {
      (seq.map(_._1), seq.map(_._2))
    }
  }

  implicit class Seq2TupleSeqOps[S, T](seq: Seq[(Seq[S], Seq[T])]) {
    def splitFlatten: (Seq[S], Seq[T]) = {
      (seq.flatMap(_._1), seq.flatMap(_._2))
    }
  }

  implicit class Seq3TupleOps[S, T, R](seq: Seq[(S, T, R)]) {
    def split: (Seq[S], Seq[T], Seq[R]) = {
      (seq.map(_._1), seq.map(_._2), seq.map(_._3))
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

  implicit class TraversableOps[T, Repr](traversable: TraversableLike[T, Repr]) {
    def mapFold[R, S, That](initial: R)(f: (R, T) => (R, S))(implicit bf: CanBuildFrom[Repr, S, That]): (R, That) = {
      traversable.foldLeft((initial, bf())) { case ((accumulator, builder), t) =>
        f(accumulator, t).mapRight(builder += _)
      }.mapRight(_.result())
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

  implicit class TraversableTryOps[T, Repr](traversable: TraversableLike[Try[T], Repr]) {
    def traverseTry[That](implicit bf: CanBuildFrom[Repr, T, That]): Try[That] = {
      traversable.foldLeft(Try(bf())) { case (builderTry, valueTry) =>
        for {
          builder <- builderTry
          value <- valueTry
        } yield builder += value
      }
    }.map(_.result())
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

    def dropUntil(p: T => Boolean): Iterator[T] = new AbstractIterator[T] {
      // Magic value: -1 = hasn't dropped, 1 = defer to parent iterator
      private[this] var status = -1
      def hasNext: Boolean =
        if (status == 1) iterator.hasNext
        else {
          while (iterator.hasNext) {
            val a = iterator.next()
            if (p(a)) {
              status = 1
              return iterator.hasNext
            }
          }
          status = 1
          false
        }
      def next(): T =
        if (hasNext) {
          iterator.next()
        }
        else Iterator.empty.next()
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
    def failIfUndefined(e: => Exception): Try[T] = x match {
      case Some(t) => Success(t)
      case None => Failure(e)
    }
  }

  implicit class OptionOptionOps[T](x: Option[Option[T]]) {
    def swap: Option[Option[T]] = x match {
      case Some(None) => None
      case None => Some(None)
      case _ => x
    }
  }

  implicit class OptionTryOps[T](x: Option[Try[T]]) {
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

  implicit class SeqStringOps(seq: Seq[String]) {
    def indent: Seq[String] = seq.map("  " + _)
    def optionalListInParens(separator: String): Option[String] = {
      Some(seq).filter(_.nonEmpty).map(_.mkString(separator).inParens)
    }
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
    def replace(key: S, f: T => T): Map[S, T] = {
      map.get(key).map(t => map.updated(key, f(t))) getOrElse map
    }
  }

  implicit class MapOptionOps[S, T](map: Map[S, Option[T]]) {
    def traverseOption: Option[Map[S, T]] = {
      map.map { case (s, tOption) => tOption.map(s -> _) }
        .traverseOption
        .map(_.toMap)
    }
  }

  implicit class TraversableOnceOps[A](coll: TraversableOnce[A]) {
    def toMapPreservingEarliest[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
      val b = immutable.Map.newBuilder[T, U]
      for (x <- coll)
        if (!b.result().contains(x._1)) b += x
      b.result()
    }
    def toSeqMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, Seq[U]] = {
      val b = immutable.Map.newBuilder[T, mutable.Builder[U, Seq[U]]]
      for (x <- coll) {
        if (!b.result().contains(x._1)) {
          b += (x._1 -> Seq.newBuilder[U])
        }
        b.result()(x._1) += x._2
      }
      b.result().mapValues(_.result())
    }
  }

  implicit class SeqEitherOps[A, B](seq: Seq[Either[A, B]]) {
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
