package net.prover.util

trait Swapper {
  def isReversed: Boolean
  def swapTuple[T](tuple: (T, T)): (T, T)
  def swap[T](l: T, r: T): (T, T) = swapTuple((l, r))
  def reverse[T](seq: Seq[T]): Seq[T] = swap(seq, seq.reverse)._1
}
object Swapper {
  val swap: Swapper = new Swapper {
    override def isReversed: Boolean = true
    override def swapTuple[T](tuple: (T, T)): (T, T) = tuple.swap
  }
  val dontSwap: Swapper = new Swapper {
    override def isReversed: Boolean = false
    override def swapTuple[T](tuple: (T, T)): (T, T) = tuple
  }
}
