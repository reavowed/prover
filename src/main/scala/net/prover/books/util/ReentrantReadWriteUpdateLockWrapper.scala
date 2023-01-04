package net.prover.books.util

import com.googlecode.concurentlocks.{ReadWriteUpdateLock, ReentrantReadWriteUpdateLock}

class ReentrantReadWriteUpdateLockWrapper extends ReadWriteUpdateLock {
  private val innerLock = new ReentrantReadWriteUpdateLock()

  val readLock = new CountingLock(innerLock.readLock())
  val updateLock = new CountingLock(innerLock.updateLock())
  val writeLock = innerLock.writeLock()

  def hasReadLock(): Boolean = readLock.getCount() > 0
  def hasUpdateLock(): Boolean = updateLock.getCount() > 0
}

