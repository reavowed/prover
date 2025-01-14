package net.prover.proving.structure.inferences

import net.prover.model.Inference
import net.prover.model.expressions.{Expression, Statement}
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.proving.extraction.StatementExtractionInference
import net.prover.proving.structure.statements.BinaryJoiner

case class Reversal[TComponent <: Expression](joiner: BinaryJoiner[TComponent], inference: Inference.Summary, inferencePremise: Statement) {
  def assertionStep(left: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Step.AssertionStep = {
    Step.AssertionStep(
      joiner(left, right),
      inference,
      Seq(Premise.Pending(joiner(right, left))),
      joiner.fillSubstitutions(Seq(right, left)))
  }

  def statementExtractionInference: StatementExtractionInference = {
    StatementExtractionInference(inference, inferencePremise, None)
  }
}

