package net.prover.model.expressions

import net.prover.model.entries.ExpressionDefinition

case class DefinitionUsages(map: Map[ExpressionDefinition, Int]) {
  def addUsage(definition: ExpressionDefinition): DefinitionUsages = {
    addUsages(definition, 1)
  }
  def addUsages(definition: ExpressionDefinition, count: Int): DefinitionUsages = {
    copy(map = map.updated(definition, map.getOrElse(definition, 0) + count))
  }

  def contains(other: DefinitionUsages): Boolean = {
    other.map.forall { case (definition, count) =>
      map.get(definition).exists(_ >= count)
    }
  }
  def -(other: DefinitionUsages): Option[DefinitionUsages] = {
    other.map.foldLeft(Option(this)) { case (usagesSoFarOption, (definition, count)) =>
      for {
        usagesSoFar <- usagesSoFarOption
        countSoFar <- usagesSoFar.map.get(definition)
        if countSoFar >= count
      } yield usagesSoFar.addUsages(definition, -count)
    }
  }


  def ++(other: DefinitionUsages): DefinitionUsages = {
    other.map.foldLeft(this) { case (usagesSoFar, (definition, count)) =>
      usagesSoFar.addUsages(definition, count)
    }
  }
}

object DefinitionUsages {
  val empty: DefinitionUsages = DefinitionUsages(Map.empty)
  implicit class DefinitionUsageSeqOps(seq: Seq[DefinitionUsages]) {
    def foldTogether: DefinitionUsages = {
      seq.fold(empty)(_ ++ _)
    }
  }
}
