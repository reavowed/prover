package net.prover.controllers

import net.prover.controllers.models._
import net.prover.model.TestDefinitions._
import net.prover.model.definitions.StatementDefinition
import net.prover.model.expressions.{Statement, StatementVariable, Term, TermVariable}
import net.prover.model.proof._
import net.prover.model.{AvailableEntries, Inference, Substitutions, VariableDefinitions}
import net.prover.proving.extraction.ExtractionCalculator
import net.prover.{BookServiceHelper, ContextHelper, StepHelpers}
import org.specs2.matcher.ValueChecks
import org.specs2.mutable.Specification

trait ControllerSpec extends Specification with ContextHelper with BookServiceHelper with StepHelpers with ValueChecks {

  def definitionWithInference(
    inference: Inference,
    statements: Seq[Statement],
    terms: Seq[Term],
    extractionInferences: Seq[Inference],
    unwrappers: Seq[StatementDefinition] = Nil,
    premisesOption: Option[Seq[Statement]] = None,
    conclusionOption: Option[Statement] = None)(
    implicit availableEntries: AvailableEntries
  ): StepDefinition = {
    val extraction = ExtractionCalculator.getInferenceExtractions(inference).find(_.extractionInferences == extractionInferences).get
    val substitutions = Substitutions(statements, terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.map(_.serialized), substitutions.terms.map(_.serialized))
    StepDefinition(
      Some(inference.id),
      None,
      serializedSubstitutions,
      extractionInferences.map(_.id),
      unwrappers.map(_.symbol),
      premisesOption.map(_.map(_.serialized)),
      conclusionOption.map(_.serialized),
      Some(extraction.additionalVariableNames))
  }
  def definitionWithPremise(
    premise: Statement,
    terms: Seq[Term],
    extractionInferences: Seq[Inference],
    conclusionOption: Option[Statement])(
    implicit availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): StepDefinition = {
    val substitutions = Substitutions(
      variableDefinitions.statements.mapWithIndex((d, i) => StatementVariable(i, (0 until d.arity).map($(_)))),
      variableDefinitions.terms.mapWithIndex((d, i) => TermVariable(i, (0 until d.arity).map($(_)))) ++ terms)
    definitionWithPremise(premise, extractionInferences, substitutions, conclusionOption)
  }
  def definitionWithPremise(
    premise: Statement,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    conclusionOption: Option[Statement])(
    implicit availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): StepDefinition = {
    implicit val stepContext: StepContext = createOuterStepContext(Nil)
    val extraction = ExtractionCalculator.getPremiseExtractions(premise).find(_.extractionInferences == extractionInferences).get
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.map(_.serialized), substitutions.terms.map(_.serialized))
    StepDefinition(
      None,
      Some(premise.serialized),
      serializedSubstitutions,
      extractionInferences.map(_.id),
      Nil,
      None,
      conclusionOption.map(_.serialized),
      Some(extraction.additionalVariableNames))
  }

  def rewrite(
    inference: Inference,
    path: Seq[Int],
    extractionInferences: Seq[Inference]
  ): RewriteRequest = {
    RewriteRequest(path, Some(inference.id), None, extractionInferences.map(_.id))
  }
  def rewrite(
    premise: Statement,
    path: Seq[Int]
  ): RewriteRequest = {
    RewriteRequest(path, None, Some(premise.serialized), Nil)
  }

}
