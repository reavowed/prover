package net.prover.proving.extraction

import net.prover.model.*
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.model.utils.ExpressionUtils

import scala.Ordering.Implicits.*

/**
  * Inferences of a regular kind that take a compound premise to a simpler conclusion using an auxiliary premise.
  *   - `φ → ψ, φ ⊢ ψ` (Modus Ponens)
  */
case class DoubleStatementExtractionInference(
  inference: Inference,
  extractionPremise: Statement,
  otherPremise: Statement)
{
  def applyToStatement(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[Step.AssertionStep] = {
    for {
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(statement).flatMap(_.confirmTotality(inference.variableDefinitions))
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
      newPremise <- otherPremise.applySubstitutions(extractionSubstitutions)
      assertionStep = Step.AssertionStep(extractedConclusion, inference.summary, Seq(statement, newPremise).map(Premise.Pending), extractionSubstitutions)
    } yield assertionStep
  }
}

object DoubleStatementExtractionInference {
  def fromInference(inference: Inference): Option[DoubleStatementExtractionInference] = inference match {
    case inference @ Inference(_, Seq(firstPremise, secondPremise), conclusion) if isValidDoubleStatementExtraction(firstPremise, secondPremise, conclusion) =>
      Some(DoubleStatementExtractionInference(inference, firstPremise, secondPremise))
    case _ =>
      None
  }

  private def isValidDoubleStatementExtraction(mainPremise: Statement, subsidiaryPremise: Statement, conclusion: Statement): Boolean = {
    conclusion.complexity < mainPremise.complexity && (for {
      subsidiaryPremiseVariable <- ExpressionUtils.getWrappedSimpleStatementVariable(subsidiaryPremise)
      conclusionVariable <- ExpressionUtils.getWrappedSimpleStatementVariable(conclusion)
      if subsidiaryPremiseVariable != conclusionVariable
      if ExpressionUtils.getWrappedBinaryStatementVariables(mainPremise).exists { t => t == (subsidiaryPremiseVariable, conclusionVariable) || t == (conclusionVariable, subsidiaryPremiseVariable)}
    } yield true).getOrElse(false)
  }
}
