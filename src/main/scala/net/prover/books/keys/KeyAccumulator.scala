package net.prover.books.keys

case class KeyAccumulator(countsByKey: Map[String, Int]) {
  def getNextKey(baseKey: String): (String, KeyAccumulator) = {
    val number = countsByKey.getOrElse(baseKey, 0) + 1
    val key = if (number == 1) baseKey else s"$baseKey-$number"
    (key, KeyAccumulator(countsByKey.updated(baseKey, number)))
  }
  def getNextKey[T: WithKeyProperty](t: T): (String, KeyAccumulator) = getNextKey(WithKeyProperty[T].getKey(t))
}

object KeyAccumulator {
  val Empty = KeyAccumulator(Map.empty)
}
