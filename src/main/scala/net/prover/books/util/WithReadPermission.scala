package net.prover.books.util

object WithReadPermission {
  def apply[T](lock: ReentrantReadWriteUpdateLockWrapper)(t: => T): T = {
    if (lock.hasUpdateLock()) {
      t
    } else {
      WithLock(lock.readLock)(t)
    }
  }
}
