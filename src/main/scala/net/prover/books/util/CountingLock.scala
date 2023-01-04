package net.prover.books.util

import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.{Condition, Lock}

class CountingLock(val innerLock: Lock) extends Lock {
  private val count = new ThreadLocal[Int]()

  def getCount(): Int = count.get()

  override def lock(): Unit = {
    innerLock.lock()
    count.set(count.get() + 1)
  }

  override def lockInterruptibly(): Unit = {
    innerLock.lockInterruptibly()
    count.set(count.get() + 1)
  }

  override def tryLock(): Boolean = {
    val result = innerLock.tryLock()
    if (result) {
      count.set(count.get() + 1)
    }
    result
  }

  override def tryLock(time: Long, unit: TimeUnit): Boolean = {
    val result = innerLock.tryLock(time, unit)
    if (result) {
      count.set(count.get() + 1)
    }
    result
  }

  override def unlock(): Unit = {
    innerLock.unlock()
    count.set(count.get() - 1)
  }

  override def newCondition(): Condition = innerLock.newCondition()
}
