package net.prover.proving.extraction

import net.prover.model.*
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.model.utils.ExpressionUtils

import scala.Ordering.Implicits.*

/**
 * Inferences of a regular kind that take a premise to a simpler conclusion. Examples:
 *   - Single statement simplifications: e.g. `¬¬φ ⊢ φ` (Remove Double Negative)
 *   - Double statement simplifications: e.g. `φ ↔ ψ ⊢ φ → ψ` (Forward Implication from Equivalence)
 *   - Double to single statement simplifications: e.g. `φ ⋀ ψ ⊢ φ` (Extract Left Conjunct)
 */
case class SingleStatementExtractionInference(
  inference: Inference,
  premise: Statement,
) {
  def applyToStatement(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[Step.AssertionStep] = {
    for {
      extractionSubstitutions <- premise.calculateSubstitutions(statement).flatMap(_.confirmTotality(inference.variableDefinitions))
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
      assertionStep = Step.AssertionStep(extractedConclusion, inference.summary, Seq(Premise.Pending(statement)), extractionSubstitutions)
    } yield assertionStep
  }
}

object SingleStatementExtractionInference {
  def fromInference(inference: Inference): Option[SingleStatementExtractionInference] = inference match {
    case inference @ Inference(_, Seq(singlePremise), conclusion)
      if isValidSinglePremiseExtraction(singlePremise, conclusion)
    =>
      Some(SingleStatementExtractionInference(inference, singlePremise))
    case _ =>
      None
  }

  private def isValidSinglePremiseExtraction(premise: Statement, conclusion: Statement): Boolean = {
    def isSingleStatementSimplification = ExpressionUtils.getWrappedSimpleStatementVariable(premise).exists(ExpressionUtils.getWrappedSimpleStatementVariable(conclusion).contains)
    def isDoubleStatementSimplification = ExpressionUtils.getWrappedBinaryStatementVariables(premise).map(_.toSet).exists(ExpressionUtils.getWrappedBinaryStatementVariables(conclusion).map(_.toSet).contains)
    def isDoubleToSingleStatementSimplification = ExpressionUtils.getWrappedBinaryStatementVariables(premise).map(_.toSet).exists { v => ExpressionUtils.getWrappedSimpleStatementVariable(conclusion).exists(v.contains) }
    conclusion.complexity < premise.complexity && (isSingleStatementSimplification || isDoubleStatementSimplification || isDoubleToSingleStatementSimplification)
  }
}
