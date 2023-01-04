package net.prover.books.util

import java.util.concurrent.locks.Lock

object WithLock {
  def apply[T](lock: Lock)(t: => T): T = {
    lock.lock()
    try {
      t
    } finally {
      lock.unlock()
    }
  }
}
