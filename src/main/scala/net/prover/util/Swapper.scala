package net.prover.util

trait Swapper {
  def isReversed: Boolean
  def swapTuple[T](tuple: (T, T)): (T, T)
  def swap[T](l: T, r: T): (T, T) = swapTuple((l, r))
  def getOne[T](l: T, r: T): T = swap(l, r)._1
  def reverse[T](seq: Seq[T]): Seq[T] = getOne(seq, seq.reverse)
  def combine(swapper: Swapper): Swapper
}
object Swapper {
  object Swap extends Swapper {
    override def isReversed: Boolean = true
    override def swapTuple[T](tuple: (T, T)): (T, T) = tuple.swap
    override def combine(swapper: Swapper): Swapper = if (swapper == Swap) DontSwap else Swap
  }
  object DontSwap extends Swapper {
    override def isReversed: Boolean = false
    override def swapTuple[T](tuple: (T, T)): (T, T) = tuple
    override def combine(swapper: Swapper): Swapper = swapper
  }
}
