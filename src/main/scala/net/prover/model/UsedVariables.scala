package net.prover.model

case class UsedVariable(index: Int, arity: Int)
object UsedVariable {
  implicit class SeqOps(usedVariables: Seq[UsedVariable]) {
    def variableIndices: Seq[Int] = usedVariables.map(_.index)
  }
}

case class UsedVariables(statements: Seq[UsedVariable], terms: Seq[UsedVariable]) {
  def contains(other: UsedVariables): Boolean = {
    other.statements.toSet.subsetOf(statements.toSet) && other.terms.toSet.subsetOf(terms.toSet)
  }
  def usesAll(variableDefinitions: VariableDefinitions): Boolean = {
    statements.variableIndices.toSet == variableDefinitions.statements.indices.toSet &&
      terms.variableIndices.toSet == variableDefinitions.terms.indices.toSet
  }
  def isEquivalentTo(other: UsedVariables): Boolean = {
    other.statements.toSet == statements.toSet && other.terms.toSet == terms.toSet
  }
}

object UsedVariables {
  val empty = UsedVariables(Nil, Nil)
  implicit class SeqOps(seq: Seq[UsedVariables]) {
    def foldTogether: UsedVariables = UsedVariables(seq.flatMap(_.statements).distinct, seq.flatMap(_.terms).distinct)
  }
}
