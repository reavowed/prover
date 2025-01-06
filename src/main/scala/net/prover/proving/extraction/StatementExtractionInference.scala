package net.prover.proving.extraction

import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.utils.ExpressionUtils
import scala.Ordering.Implicits._

/**
  * Inferences of a regular kind that take statements to simpler statements. Examples:
  *   - Single statement simplifications: e.g. `¬¬φ ⊢ φ` (Remove Double Negative)
  *   - Double statement simplifications: e.g. `φ ↔ ψ ⊢ φ → ψ` (Forward Implication from Equivalence)
  *   - Double to single statement simplifications: e.g. `φ ⋀ ψ ⊢ φ` (Extract Left Conjunct)
  *   - Double statement simplification with auxiliary: e.g. `φ → ψ, φ ⊢ ψ` (Modus Ponens)
  */
case class StatementExtractionInference(
  inference: Inference,
  extractionPremise: Statement,
  otherPremiseOption: Option[Statement])

object StatementExtractionInference {
  def fromInference(inference: Inference): Option[StatementExtractionInference] = inference match {
    case inference @ Inference(_, Seq(singlePremise), conclusion)
      if isValidSinglePremiseExtraction(singlePremise, conclusion)
    =>
      Some(StatementExtractionInference(inference, singlePremise, None))
    case inference @ Inference(_, Seq(firstPremise, secondPremise), conclusion) if isValidDoublePremiseExtraction(firstPremise, secondPremise, conclusion) =>
      Some(StatementExtractionInference(inference, firstPremise, Some(secondPremise)))
    case _ =>
      None
  }

  private def isValidSinglePremiseExtraction(premise: Statement, conclusion: Statement): Boolean = {
    def isSingleStatementSimplification = ExpressionUtils.getWrappedSimpleStatementVariable(premise).exists(ExpressionUtils.getWrappedSimpleStatementVariable(conclusion).contains)
    def isDoubleStatementSimplification = ExpressionUtils.getWrappedBinaryStatementVariables(premise).map(_.toSet).exists(ExpressionUtils.getWrappedBinaryStatementVariables(conclusion).map(_.toSet).contains)
    def isDoubleToSingleStatementSimplification = ExpressionUtils.getWrappedBinaryStatementVariables(premise).map(_.toSet).exists { v => ExpressionUtils.getWrappedSimpleStatementVariable(conclusion).exists(v.contains) }
    conclusion.complexity < premise.complexity && (isSingleStatementSimplification || isDoubleStatementSimplification || isDoubleToSingleStatementSimplification)
  }
  private def isValidDoublePremiseExtraction(mainPremise: Statement, subsidiaryPremise: Statement, conclusion: Statement): Boolean = {
    conclusion.complexity < mainPremise.complexity && (for {
      subsidiaryPremiseVariable <- ExpressionUtils.getWrappedSimpleStatementVariable(subsidiaryPremise)
      conclusionVariable <- ExpressionUtils.getWrappedSimpleStatementVariable(conclusion)
      if subsidiaryPremiseVariable != conclusionVariable
      if ExpressionUtils.getWrappedBinaryStatementVariables(mainPremise).exists { t => t == (subsidiaryPremiseVariable, conclusionVariable) || t == (conclusionVariable, subsidiaryPremiseVariable)}
    } yield true).getOrElse(false)
  }
}
