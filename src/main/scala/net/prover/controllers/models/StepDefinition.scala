package net.prover.controllers.models

import scala.util.Try
import net.prover.controllers._
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstitutionContext
import net.prover.proving.extraction.ExtractionDefinition

case class StepDefinition(
    inferenceId: Option[String],
    serializedPremiseStatement: Option[String],
    substitutions: SerializedSubstitutions,
    extractionDefinition: ExtractionDefinition.Serialized,
    wrappingSymbols: Seq[String],
    serializedIntendedPremiseStatements: Option[Seq[String]],
    serializedIntendedConclusionStatement: Option[String],
    additionalVariableNames: Option[Seq[String]])
{
  def getFromInferenceOrPremise[T](fromInference: String => Try[T], fromPremise: String => Try[T]): Try[T] = {
    (inferenceId.map(fromInference) orElse serializedPremiseStatement.map(fromPremise) orBadRequest "Neither inference nor premise supplied").flatten
  }
  def parseIntendedPremiseStatements(expressionParsingContext: ExpressionParsingContext): Try[Option[Seq[Statement]]] = {
    serializedIntendedPremiseStatements.map { serializedStatements =>
      serializedStatements.mapWithIndex { (s, i) => Statement.parser(expressionParsingContext).parseFromString(s, s"new target statement ${i + 1}").recoverWithBadRequest }.traverseTry
    }.swap
  }
  def parseIntendedConclusion(expressionParsingContext: ExpressionParsingContext, substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Try[Option[Statement]] = {
    serializedIntendedConclusionStatement.map { serializedIntendedConclusion =>
      for {
        conclusionStatement <- Statement.parser(expressionParsingContext).parseFromString(serializedIntendedConclusion, "conclusion").recoverWithBadRequest
        substitutedConclusionStatement <- conclusionStatement.applySubstitutions(substitutions).orBadRequest("Could not apply substitutions to intended conclusion")
      } yield substitutedConclusionStatement
    }.swap
  }
}
