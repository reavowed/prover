package net.prover.controllers

import net.prover.controllers.models._
import net.prover.model.TestDefinitions._
import net.prover.model.definitions.StatementDefinition
import net.prover.model.expressions.{Statement, StatementVariable, Term, TermVariable}
import net.prover.model.proof._
import net.prover.model.{AvailableEntries, Inference, Substitutions, VariableDefinitions}
import net.prover.proving.extraction.{ExtractionCalculator, ExtractionDefinition}
import net.prover.{BookServiceHelper, ContextHelper, StepHelpers}
import org.specs2.matcher.ValueChecks
import org.specs2.mutable.Specification

trait ControllerSpec extends Specification with ContextHelper with BookServiceHelper with StepHelpers with ValueChecks {

  def definitionWithInference(
    inference: Inference,
    statements: Seq[Statement],
    terms: Seq[Term],
    extractionDefinition: ExtractionDefinition,
    unwrappers: Seq[StatementDefinition] = Nil,
    premisesOption: Option[Seq[Statement]] = None,
    conclusionOption: Option[Statement] = None)(
    implicit availableEntries: AvailableEntries
  ): StepDefinition = {
    val serializedExtractionDefinition = extractionDefinition.serialized
    val extraction = ExtractionCalculator.getInferenceExtractions(inference).find(_.extractionDefinition.matches(serializedExtractionDefinition)).get
    val substitutions = Substitutions(statements, terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.map(_.serialized), substitutions.terms.map(_.serialized))
    StepDefinition(
      Some(inference.id),
      None,
      serializedSubstitutions,
      serializedExtractionDefinition,
      unwrappers.map(_.symbol),
      premisesOption.map(_.map(_.serialized)),
      conclusionOption.map(_.serialized),
      Some(extraction.additionalVariableNames))
  }
  def definitionWithPremise(
    premise: Statement,
    terms: Seq[Term],
    extractionDefinition: ExtractionDefinition,
    conclusionOption: Option[Statement])(
    implicit availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): StepDefinition = {
    val substitutions = Substitutions(
      variableDefinitions.statements.mapWithIndex((d, i) => StatementVariable(i, (0 until d.arity).map($(_)))),
      variableDefinitions.terms.mapWithIndex((d, i) => TermVariable(i, (0 until d.arity).map($(_)))) ++ terms)
    definitionWithPremise(premise, extractionDefinition, substitutions, conclusionOption)
  }
  def definitionWithPremise(
    premise: Statement,
    extractionDefinition: ExtractionDefinition,
    substitutions: Substitutions,
    conclusionOption: Option[Statement])(
    implicit availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): StepDefinition = {
    implicit val stepContext: StepContext = createOuterStepContext(Nil)
    val serializedExtractionDefinition = extractionDefinition.serialized
    val extraction = ExtractionCalculator.getPremiseExtractions(premise).find(_.extractionDefinition.matches(serializedExtractionDefinition)).get
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.map(_.serialized), substitutions.terms.map(_.serialized))
    StepDefinition(
      None,
      Some(premise.serialized),
      serializedSubstitutions,
      serializedExtractionDefinition,
      Nil,
      None,
      conclusionOption.map(_.serialized),
      Some(extraction.additionalVariableNames))
  }

  def rewrite(
    inference: Inference,
    path: Seq[Int],
    extractionDefinition: ExtractionDefinition
  ): RewriteRequest = {
    RewriteRequest(path, Some(inference.id), None, extractionDefinition.serialized)
  }
  def rewrite(
    premise: Statement,
    path: Seq[Int]
  ): RewriteRequest = {
    RewriteRequest(path, None, Some(premise.serialized), ExtractionDefinition.Empty.serialized)
  }

  def simpleExtraction(extractionInferences: Inference*): ExtractionDefinition = {
    ExtractionDefinition(extractionInferences.map(_.summary), None)
  }
}
