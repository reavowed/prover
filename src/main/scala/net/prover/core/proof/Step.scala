package net.prover.core.proof

import net.prover.core.{ImplicationDefinition, RuleOfInference, UniversalQuantificationDefinition}
import net.prover.core.expressions.Statement
import net.prover.core.substitutions.Substitutions

sealed trait Step {
  def statement: Statement
}

case class RuleOfInferenceApplicationStep(statement: Statement, ruleOfInference: RuleOfInference, substitutions: Substitutions) extends Step
case class ImplicationIntroductionStep(antecedent: Statement, substeps: ::[Step], implicationDefinition: ImplicationDefinition) extends Step {
  def statement: Statement = implicationDefinition(antecedent, substeps.last.statement)
}
case class UniversalQuantificationIntroductionStep(variableName: String, substeps: ::[Step], universalQuantificationDefinition: UniversalQuantificationDefinition) extends Step {
  def statement: Statement = universalQuantificationDefinition(variableName, substeps.last.statement)
}
