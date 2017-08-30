package net.prover.model.proof

case class ReferenceMap(map: Map[String, Set[Reference.ToFact]]) {
  def ++(other: ReferenceMap) = {
    val mergedMap = (map.keySet ++ other.map.keySet).map { key =>
      key -> (map.getOrElse(key, Nil) ++ other.map.getOrElse(key, Nil)).toSet
    }.toMap
    ReferenceMap(mergedMap)
  }
  def getReferrers(values: String*): Set[(String, Seq[Int])] = {
    map.keySet.flatMap { key =>
      map(key).filter(ref => values.contains(ref.valueAndPath._1)).map(key -> _.valueAndPath._2)
    }
  }
}

object ReferenceMap {
  val empty = ReferenceMap()
  implicit class ReferenceMapSeqOps(seq: Seq[ReferenceMap]) {
    def foldTogether: ReferenceMap = {
      seq.foldLeft(empty)(_ ++ _)
    }
  }
  def apply(tuples: (String, Set[Reference.ToFact])*): ReferenceMap = ReferenceMap(Map(tuples: _*))
}
