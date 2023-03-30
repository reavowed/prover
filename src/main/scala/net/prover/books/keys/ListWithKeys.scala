package net.prover.books.keys

case class ListWithKeys[T : WithKeyProperty](listWithKeys: List[(T, String)], keyAccumulator: KeyAccumulator) {
  def :+(newItem: T): ListWithKeys[T] = {
    val (key, newAccumulator) = keyAccumulator.getNextKey(WithKeyProperty[T].getKeyProperty(newItem))
    ListWithKeys(
      listWithKeys :+ (newItem, key),
      newAccumulator)
  }
  def -(t: T): ListWithKeys[T] = {
    ListWithKeys(list.filter(_ != t))
  }
  def updated(key: String, t: T): ListWithKeys[T] = {
    copy(listWithKeys = listWithKeys.map(x => if (x._2 == key) (t, key) else x))
  }
  def list: List[T] = listWithKeys.map(_._1)
}

object ListWithKeys {
  def empty[S : WithKeyProperty]: ListWithKeys[S] = ListWithKeys(Nil, KeyAccumulator.Empty)
  def apply[T : WithKeyProperty](list: List[T]) = list.foldLeft(ListWithKeys.empty[T]) { (listWithKeys, t) => listWithKeys :+ t }
}
