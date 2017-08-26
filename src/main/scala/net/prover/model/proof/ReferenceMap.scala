package net.prover.model.proof

case class ReferenceMap(map: Map[String, Set[String]]) {
  def ++(other: ReferenceMap) = {
    val mergedMap = (map.keySet ++ other.map.keySet).map { key =>
      key -> (map.getOrElse(key, Nil) ++ other.map.getOrElse(key, Nil)).toSet
    }.toMap
    ReferenceMap(mergedMap)
  }
  def getReferrers(values: String*): Set[String] = {
    map.keySet.filter { key => map(key).exists(values.contains) }
  }
}

object ReferenceMap {
  val empty = ReferenceMap()
  implicit class ReferenceMapSeqOps(seq: Seq[ReferenceMap]) {
    def foldTogether: ReferenceMap = {
      seq.foldLeft(empty)(_ ++ _)
    }
  }
  def apply(tuples: (String, Set[String])*): ReferenceMap = ReferenceMap(Map(tuples: _*))
}
