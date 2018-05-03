package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model._

sealed trait Step {
  def reference: Reference.Direct
  def provenStatements: Seq[ProvenStatement]
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
  def cached: CachedStep
  def length: Int
  def intermediateReferences: Seq[String]
  def lastReference: Option[String]
  def getLines(
    referenceMap: ReferenceMap,
    indentLevel: Int,
    additionalReference: Option[String])(
    implicit displayContext: DisplayContext
  ): Seq[ProofLine]
  def isSingleAssertion: Boolean = false
  def serializedLines: Seq[String]
}

object Step {
  case class Assertion(
      assertion: Statement,
      inferenceApplication: InferenceApplication,
      reference: Reference.Direct)
    extends Step
  {
    override def provenStatements = Seq(ProvenStatement(assertion, reference))
    override def referencedInferenceIds = inferenceApplication.referencedInferenceIds
    override def referenceMap = ReferenceMap(reference.value -> inferenceApplication.lineReferences)
    override def cached = CachedStep.Assertion(assertion, inferenceApplication.cached, reference)
    override def length = 1
    override def intermediateReferences = Nil
    override def lastReference = Some(reference.value)
    override def getLines(
      referenceMap: ReferenceMap,
      indentLevel: Int,
      additionalReference: Option[String])(
      implicit displayContext: DisplayContext
    ) = {
      Seq(ProofLine(
        "Then",
        ProofLine.Expression.create(assertion, referenceMap.getReferrers(reference.value, additionalReference)),
        Some(reference.value),
        indentLevel,
        if (inferenceApplication.isRearrangement)
          Some(ProofLine.InferenceLink("Rearrangement", None))
        else
          Some(ProofLine.InferenceLink(HtmlHelper.findInferenceToDisplay(inferenceApplication)))))
    }
    override def isSingleAssertion = true

    def elidedStatementOption = inferenceApplication.references.ofType[Reference.Elided].headOption.map(_.inferenceApplication.conclusion)
    override def serializedLines = Seq(s"prove ${assertion.serialized}") ++
      elidedStatementOption.map(s => s"via ${s.serialized}").toSeq.indent
  }

  case class Assumption(
      assumption: Statement,
      substeps: Seq[Step],
      deductionStatement: StatementDefinition,
      reference: Reference.Direct)
    extends Step
  {
    override def provenStatements = substeps.flatMap(_.provenStatements).map { innerProvenStatement =>
      ProvenStatement(
        DefinedStatement(Seq(assumption, innerProvenStatement.statement), deductionStatement, assumption.depth)(Nil),
        innerProvenStatement.reference.add(reference))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.Assumption(assumption, substeps.map(_.cached), reference)
    override def length = substeps.map(_.length).sum
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = substeps.lastOption.flatMap(_.lastReference)
    override def getLines(
      referenceMap: ReferenceMap,
      indentLevel: Int,
      additionalReference: Option[String])(
      implicit displayContext: DisplayContext
    ) = {
      val substepLines = substeps.flatMapWithIndex((step, index) =>
        step.getLines(referenceMap, indentLevel + 1, if (index == substeps.length - 1) additionalReference else None))
      if (isSingleAssertion) {
        substepLines.map { substepLine =>
          ProofLine(
            "Then",
            ProofLine.Expression.Nested(
              deductionStatement.format,
              Seq(ProofLine.Expression.create(assumption, Set.empty), substepLine.expression),
              substepLine.expression.referrers),
            substepLine.reference,
            indentLevel,
            substepLine.inferenceLink)
        }
      } else {
        val assumptionLine = ProofLine(
          "Assume",
          ProofLine.Expression.create(assumption, referenceMap.getReferrers(reference.value, additionalReference)),
          None,
          indentLevel,
          None)
        assumptionLine +: substepLines
      }
    }
    override def isSingleAssertion = substeps match {
      case Seq(singleStep) if singleStep.isSingleAssertion => true
      case _ => false
    }
    override def serializedLines = Seq(s"assume ${assumption.serialized} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
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
    override def intermediateReferences = assumptionStep.intermediateReferences
    override def lastReference = Some(reference.value)
    override def getLines(
      referenceMap: ReferenceMap,
      indentLevel: Int,
      additionalReference: Option[String])(
      implicit displayContext: DisplayContext
    ) = {
      val firstLine = ProofLine(
        s"Let $variableName be such that",
        ProofLine.Expression.create(assumptionStep.assumption, referenceMap.getReferrers(assumptionStep.reference.value)),
        Some(assertionStep.reference.value),
        indentLevel,
        Some(ProofLine.InferenceLink(HtmlHelper.findInferenceToDisplay(assertionStep.inferenceApplication))))
      val innerLines = assumptionStep.substeps.flatMapWithIndex((step, index) =>
        step.getLines(
          referenceMap,
          indentLevel,
          if (index == assumptionStep.substeps.length - 1) Some(additionalReference.getOrElse(reference.value)) else None))
      firstLine +: innerLines
    }
    override def serializedLines = Seq(s"let $variableName ${assumptionStep.assumption.serialized} {") ++
      assumptionStep.substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
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
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = substeps.lastOption.flatMap(_.lastReference)
    override def getLines(
      referenceMap: ReferenceMap,
      indentLevel: Int,
      additionalReference: Option[String])(
      implicit displayContext: DisplayContext
    ) = {
      val substepLines = substeps.flatMapWithIndex { (step, index) =>
        step.getLines(referenceMap, indentLevel, if (index == substeps.length - 1) additionalReference else None)
      }
      if (isSingleAssertion)
        substepLines.map { substepLine =>
          substepLine.copy(expression = ProofLine.Expression.Nested(
            scopingStatement.format,
            Seq(ProofLine.Expression.Plain(variableName, Set.empty), substepLine.expression),
            substepLine.expression.referrers))
        }
      else
        substepLines
    }
    override def isSingleAssertion = substeps match {
      case Seq(singleStep) if singleStep.isSingleAssertion => true
      case _ => false
    }
    override def serializedLines = Seq(s"take $variableName {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }

  implicit class StepSeqOps(steps: Seq[Step]) {
    def intermediateReferences: Seq[String] = {
      steps.dropRight(1).flatMap { step =>
        step.intermediateReferences ++ step.lastReference.toSeq
      } ++ steps.lastOption.toSeq.flatMap(_.intermediateReferences)
    }
  }
}
