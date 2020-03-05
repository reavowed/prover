package net.prover.controllers.models

import scala.util.Try
import net.prover.controllers._

case class StepDefinition(
    inferenceId: Option[String],
    serializedPremiseStatement: Option[String],
    substitutions: SerializedSubstitutions,
    extractionInferenceIds: Seq[String],
    serializedInferencePremiseStatements: Option[Seq[String]],
    serializedConclusionStatement: Option[String],
    additionalVariableNames: Option[Seq[String]])
{
  def getFromInferenceOrPremise[T](fromInference: String => Try[T], fromPremise: String => Try[T]): Try[T] = {
    (inferenceId.map(fromInference) orElse serializedPremiseStatement.map(fromPremise) orBadRequest "Neither inference nor premise supplied").flatten
  }
}
