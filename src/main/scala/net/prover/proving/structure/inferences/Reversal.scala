package net.prover.proving.structure.inferences

import net.prover.model.Inference
import net.prover.model.expressions.{Expression, Statement}
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.proving.derivation.DerivedStatement
import net.prover.proving.extraction.{DoubleStatementExtractionInference, SingleStatementExtractionInference}
import net.prover.proving.structure.statements.BinaryJoiner

case class Reversal[TComponent <: Expression](joiner: BinaryJoiner[TComponent], inference: Inference.Summary, inferencePremise: Statement) {
  def assertionStep(left: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Step.AssertionStep = {
    Step.AssertionStep(
      joiner(left, right),
      inference,
      Seq(Premise.Pending(joiner(right, left))),
      joiner.fillSubstitutions(Seq(right, left)))
  }

  /**
    * Reverses a derived statement joined by this reversal's joiner, e.g. "a = b" to "b = a".
    */
  def reverse(derivation: DerivedStatement)(implicit substitutionContext: SubstitutionContext): Option[DerivedStatement] = {
    for {
      (left, right) <- joiner.unapply(derivation.statement)
    } yield DerivedStatement.Reversed(derivation, assertionStep(right, left))
  }

  def statementExtractionInference: SingleStatementExtractionInference = {
    SingleStatementExtractionInference(inference, inferencePremise)
  }
}

