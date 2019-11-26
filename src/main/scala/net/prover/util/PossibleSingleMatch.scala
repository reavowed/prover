package net.prover.util

sealed trait PossibleSingleMatch[+T] {
  def toOption: Option[T]
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def map[S](f: T => S): PossibleSingleMatch[S]
  def flatMap[S](f: T => PossibleSingleMatch[S]): PossibleSingleMatch[S]
  def filter(f: T => Boolean): PossibleSingleMatch[T]
  def orElse[S >: T](other: => PossibleSingleMatch[S]): PossibleSingleMatch[S]
}

object PossibleSingleMatch {
  sealed trait Empty extends PossibleSingleMatch[Nothing] {
    def toOption: Option[Nothing] = scala.None
    def isEmpty: Boolean = true
    def map[S](f: Nothing => S): PossibleSingleMatch[S] = this
    def flatMap[S](f: Nothing => PossibleSingleMatch[S]): PossibleSingleMatch[S] = this
    def filter(f: Nothing => Boolean): PossibleSingleMatch[Nothing] = this
  }

  case object NoMatches extends Empty {
    def orElse[S >: Nothing](other: => PossibleSingleMatch[S]): PossibleSingleMatch[S] = other
  }
  case class SingleMatch[T](value: T) extends PossibleSingleMatch[T] {
    def toOption: Option[T] = Some(value)
    def isEmpty: Boolean = false
    def map[S](f: T => S): PossibleSingleMatch[S] = SingleMatch(f(value))
    def flatMap[S](f: T => PossibleSingleMatch[S]): PossibleSingleMatch[S] = f(value)
    def filter(f: T => Boolean): PossibleSingleMatch[T] = if (f(value)) this else NoMatches
    def orElse[S >: T](other: => PossibleSingleMatch[S]): PossibleSingleMatch[S] = other match {
      case NoMatches => this
      case SingleMatch(`value`) => this
      case _ => MultipleMatches
    }
  }
  case object MultipleMatches extends Empty {
    def orElse[S >: Nothing](other: =>PossibleSingleMatch[S]): PossibleSingleMatch[S] = this
  }

  implicit class SeqPossibleOptionOps[T](seq: Seq[PossibleSingleMatch[T]]) {
    def traverse: PossibleSingleMatch[Seq[T]] = seq.foldLeft[PossibleSingleMatch[Seq[T]]](SingleMatch(Nil)) {
      case (SingleMatch(seq), SingleMatch(t)) => SingleMatch(seq :+ t)
      case (NoMatches, _) => NoMatches
      case (_, NoMatches) => NoMatches
      case _ => MultipleMatches
    }
  }
}

