package net.prover.books.keys

case class ListWithKeys[T : WithKeyProperty](listWithKeys: List[(T, String)], keyAccumulator: KeyAccumulator) {
  def addAndGetKey(newItem: T): (ListWithKeys[T], String) = {
    val (key, newAccumulator) = keyAccumulator.getNextKey(newItem)
    val newList = ListWithKeys(listWithKeys :+ (newItem, key), newAccumulator)
    (newList, key)
  }
  def :+(newItem: T): ListWithKeys[T] = {
    addAndGetKey(newItem)._1
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
