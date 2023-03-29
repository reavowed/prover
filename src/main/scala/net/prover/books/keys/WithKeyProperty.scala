package net.prover.books.keys

trait WithKeyProperty[T] {
  def getKeyProperty(t: T): String
}

object WithKeyProperty {
  def apply[T](implicit withKeyProperty: WithKeyProperty[T]): WithKeyProperty[T] = withKeyProperty
}
