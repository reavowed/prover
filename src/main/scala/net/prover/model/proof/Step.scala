package net.prover.model.proof

import net.prover.model.HtmlHelper
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}

sealed trait Step {
  def reference: Reference.Direct
  def provenStatements: Seq[ProvenStatement]
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
  def cached: CachedStep
  def length: Int
  def intermediateReferences: Seq[String]
  def getLines(referenceMap: ReferenceMap, indentLevel: Int, additionalReference: Option[String]): Seq[ProofLine]
}

object Step {
  case class Assertion(
    assertion: Statement,
    inferenceApplication: InferenceApplication,
    reference: Reference.Direct,
    isRearrangement: Boolean)
    extends Step
  {
    override def provenStatements = Seq(ProvenStatement(assertion, reference))
    override def referencedInferenceIds = inferenceApplication.referencedInferenceIds
    override def referenceMap = ReferenceMap(reference.value -> inferenceApplication.lineReferences)
    override def cached = CachedStep.Assertion(assertion, inferenceApplication.cached, reference, isRearrangement)
    override def length = 1
    override def intermediateReferences = Seq(reference.value)
    override def getLines(referenceMap: ReferenceMap, indentLevel: Int, additionalReference: Option[String]) = {
      Seq(ProofLine(
        "Then",
        ProofLine.Expression.create(assertion, referenceMap.getReferrers(reference.value, additionalReference)),
        Some(reference.value),
        indentLevel,
        if (isRearrangement)
          Some(ProofLine.InferenceLink("Rearrangement", None))
        else
          Some(ProofLine.InferenceLink(HtmlHelper.findInferenceToDisplay(inferenceApplication)))))
    }
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[Step],
      deductionStatement: StatementDefinition,
      reference: Reference.Direct)
    extends Step
  {
    override def provenStatements = steps.flatMap(_.provenStatements).map { innerProvenStatement =>
      ProvenStatement(
        DefinedStatement(Seq(assumption, innerProvenStatement.statement), deductionStatement, assumption.depth)(Nil),
        innerProvenStatement.reference.add(reference))
    }
    override def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.Assumption(assumption, steps.map(_.cached), reference)
    override def length = steps.map(_.length).sum
    override def intermediateReferences = steps.dropRight(1).flatMap(_.intermediateReferences) :+ reference.value
    override def getLines(referenceMap: ReferenceMap, indentLevel: Int, additionalReference: Option[String]) = {
      val assumptionLine = ProofLine(
        "Assume",
        ProofLine.Expression.create(assumption, referenceMap.getReferrers(reference.value, additionalReference)),
        None,
        indentLevel,
        None)
      val innerLines = steps.flatMapWithIndex((step, index) =>
        step.getLines(referenceMap, indentLevel + 1, if (index == steps.length - 1) additionalReference else None))
      assumptionLine +: innerLines
    }
  }

  case class Naming(
      variableName: String,
      assumptionStep: Step.Assumption,
      assertionStep: Step.Assertion,
      reference: Reference.Direct)
    extends Step
  {
    override def provenStatements = Seq(ProvenStatement(assertionStep.assertion, reference))
    override def referencedInferenceIds = assumptionStep.referencedInferenceIds ++ assertionStep.referencedInferenceIds
    override def referenceMap = assumptionStep.referenceMap ++ assertionStep.referenceMap
    override def cached = CachedStep.Naming(variableName, assumptionStep.cached, assertionStep.cached, reference)
    override def length = assumptionStep.length + 1
    override def intermediateReferences = assumptionStep.steps.dropRight(1).flatMap(_.intermediateReferences) :+ reference.value
    override def getLines(referenceMap: ReferenceMap, indentLevel: Int, additionalReference: Option[String]) = {
      val firstLine = ProofLine(
        s"Let $variableName be such that",
        ProofLine.Expression.create(assumptionStep.assumption, referenceMap.getReferrers(assumptionStep.reference.value)),
        Some(assumptionStep.reference.value),
        indentLevel,
        Some(ProofLine.InferenceLink(HtmlHelper.findInferenceToDisplay(assertionStep.inferenceApplication))))
      val innerLines = assumptionStep.steps.flatMapWithIndex((step, index) =>
        step.getLines(
          referenceMap,
          indentLevel + 1,
          if (index == assumptionStep.steps.length - 1) Some(additionalReference.getOrElse(reference.value)) else None))
      firstLine +: innerLines
    }
  }

  case class ScopedVariable(
      variableName: String,
      substeps: Seq[Step],
      scopingStatement: StatementDefinition,
      reference: Reference.Direct)
    extends Step
  {
    override def provenStatements = {
      substeps.flatMap(_.provenStatements)
        .map { innerProvenStatement =>
          ProvenStatement(
            DefinedStatement(
              Seq(innerProvenStatement.statement),
              scopingStatement,
              innerProvenStatement.statement.depth - 1)(
              Seq(variableName)),
            innerProvenStatement.reference.add(reference))
        }
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.ScopedVariable(variableName, substeps.map(_.cached), reference)
    override def length = substeps.map(_.length).sum
    override def intermediateReferences = substeps.dropRight(1).flatMap(_.intermediateReferences) :+ reference.value
    override def getLines(referenceMap: ReferenceMap, indentLevel: Int, additionalReference: Option[String]) = {
      substeps.flatMapWithIndex((step, index) =>
        step.getLines(referenceMap, indentLevel, if (index == substeps.length - 1) additionalReference else None))
    }
  }
}
