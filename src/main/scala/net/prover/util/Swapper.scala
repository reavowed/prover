package net.prover.util

trait Swapper {
  def isReversed: Boolean
  def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T)
  def swapSourceAndResult[T](l: T, r: T): (T, T) = getSourceAndResultFromTuple((l, r))
  def getSource[T](l: T, r: T): T = swapSourceAndResult(l, r)._1
  def getResult[T](l: T, r: T): T = swapSourceAndResult(l, r)._2
  def reverse[T](seq: Seq[T]): Seq[T] = getSource(seq, seq.reverse)
  def combine(swapper: Swapper): Swapper
}
object Swapper {
  object Swap extends Swapper {
    override def isReversed: Boolean = true
    override def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T) = tuple.swap
    override def combine(swapper: Swapper): Swapper = if (swapper == Swap) DontSwap else Swap
  }
  object DontSwap extends Swapper {
    override def isReversed: Boolean = false
    override def getSourceAndResultFromTuple[T](tuple: (T, T)): (T, T) = tuple
    override def combine(swapper: Swapper): Swapper = swapper
  }
}
