package net.prover.books.keys

import net.prover.model.StringOps

trait WithKeyProperty[T] {
  def getKeyProperty(t: T): String
  def getKey(t: T): String = getKeyProperty(t).formatAsKey
}

object WithKeyProperty {
  def apply[T](implicit withKeyProperty: WithKeyProperty[T]): WithKeyProperty[T] = withKeyProperty
}
