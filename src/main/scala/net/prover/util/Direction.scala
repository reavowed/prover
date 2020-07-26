package net.prover.util

trait Direction {
  def isReversed: Boolean
  def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T)
  def getSource[T](l: => T, r: => T): T
  def getResult[T](l: => T, r: => T): T
  def swapSourceAndResult[T](l: T, r: T): (T, T) = getSourceAndResultFromTuple((l, r))
  def getSourceFromTuple[T](tuple: (T, T)): T = getSourceAndResultFromTuple(tuple)._1
  def reverseIfNecessary[T](seq: Seq[T]): Seq[T] = getSource(seq, seq.reverse)
  def prepend[T](t: T, seq: Seq[T]): Seq[T] = getSource(t +: seq, seq :+ t)
  def append[T](seq: Seq[T], t: T): Seq[T] = getSource(seq :+ t, t +: seq)
  def concat[T](first: Seq[T], second: Seq[T]): Seq[T] = getSource(first ++ second, second ++ first)
  def combine(otherDirection: Direction): Direction = getSource(otherDirection, otherDirection.reverse)
  def reverse: Direction
}
object Direction {
  object Forward extends Direction {
    override def isReversed: Boolean = false
    override def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T) = tuple
    override def reverse: Direction = Reverse
    override def getSource[T](l: => T, r: => T): T = l
    override def getResult[T](l: => T, r: => T): T = r
  }
  object Reverse extends Direction {
    override def isReversed: Boolean = true
    override def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T) = tuple.swap
    override def reverse: Direction = Forward
    override def getSource[T](l: => T, r: => T): T = r
    override def getResult[T](l: => T, r: => T): T = l
  }
}
