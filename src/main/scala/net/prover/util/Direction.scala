package net.prover.util

trait Direction {
  def isReversed: Boolean
  def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T)
  def swapSourceAndResult[T](l: T, r: T): (T, T) = getSourceAndResultFromTuple((l, r))
  def getSource[T](l: T, r: T): T = swapSourceAndResult(l, r)._1
  def getResult[T](l: T, r: T): T = swapSourceAndResult(l, r)._2
  def reverseIfNecessary[T](seq: Seq[T]): Seq[T] = getSource(seq, seq.reverse)
  def combine(otherDirection: Direction): Direction = getSource(otherDirection, otherDirection.reverse)
  def reverse: Direction
}
object Direction {
  object Forward extends Direction {
    override def isReversed: Boolean = false
    override def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T) = tuple
    override def reverse: Direction = Reverse
  }
  object Reverse extends Direction {
    override def isReversed: Boolean = true
    override def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T) = tuple.swap
    override def reverse: Direction = Forward
  }
}
