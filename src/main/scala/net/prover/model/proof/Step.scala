package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model._

sealed trait Step {
  def reference: Reference.Direct
  def provenStatements: Seq[ProvenStatement]
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
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
          Some(ProofLine.EntryLink("Rearrangement", None))
        else
          Some(ProofLine.EntryLink(HtmlHelper.findInferenceToDisplay(inferenceApplication)))))
    }
    override def isSingleAssertion = true

    override def serializedLines = Seq(s"assert ${assertion.serialized} ${inferenceApplication.serialized}")
  }
  object Assertion {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[Assertion] = {
      for {
        assertion <- Statement.parser
        inferenceApplication <- InferenceApplication.parser
      } yield Assertion(assertion, inferenceApplication, reference)
    }
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
        DefinedStatement(Seq(assumption, innerProvenStatement.statement), deductionStatement)(Nil),
        innerProvenStatement.reference.add(reference.getChildForAssumption))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
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
          ProofLine.Expression.create(assumption, referenceMap.getReferrers(reference.getChildForAssumption.value, additionalReference)),
          Some(reference.getChildForAssumption.value),
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
  object Assumption {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[Assumption] = {
      val deductionStatement = context.deductionStatement
        .getOrElse(throw new Exception("Cannot prove a deduction without an appropriate statement definition"))
      for {
        assumption <- Statement.parser
        substeps <- listParser(Some(reference)).inBraces
      } yield Assumption(assumption, substeps, deductionStatement, reference)
    }
  }

  case class Naming(
      variableName: String,
      assumption: Statement,
      substeps: Seq[Step],
      finalInferenceApplication: InferenceApplication,
      reference: Reference.Direct)
    extends Step
  {
    private def assumptionReference = reference.getChildForAssumption
    private def finalAssertionReference = reference.getChildForResult
    private def finalAssertion = finalInferenceApplication.conclusion

    override def provenStatements = Seq(ProvenStatement(finalAssertion, reference))
    override def referencedInferenceIds = substeps.flatMap(_.referencedInferenceIds).toSet ++ finalInferenceApplication.referencedInferenceIds
    override def referenceMap = substeps.map(_.referenceMap).foldTogether ++
      ReferenceMap(finalAssertionReference.value -> finalInferenceApplication.lineReferences)
    override def length = substeps.map(_.length).sum + 1
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = Some(reference.value)
    override def getLines(
      referenceMap: ReferenceMap,
      indentLevel: Int,
      additionalReference: Option[String])(
      implicit displayContext: DisplayContext
    ) = {
      val firstLine = ProofLine(
        s"Let $variableName be such that",
        ProofLine.Expression.create(assumption, referenceMap.getReferrers(assumptionReference.value)),
        Some(finalAssertionReference.value),
        indentLevel,
        Some(ProofLine.EntryLink(HtmlHelper.findInferenceToDisplay(finalInferenceApplication))))
      val innerLines = substeps.flatMapWithIndex((step, index) =>
        step.getLines(
          referenceMap,
          indentLevel,
          if (index == substeps.length - 1) Some(additionalReference.getOrElse(reference.value)) else None))
      firstLine +: innerLines
    }
    override def serializedLines = Seq(s"let $variableName ${assumption.serialized} ${finalInferenceApplication.serialized} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }

  object Naming {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[Naming] = {
      for {
        variableName <- Parser.singleWord
        innerContext = context.addParameters(variableName)
        assumption <- Statement.parser(innerContext)
        finalInferenceApplication <- InferenceApplication.parser
        substeps <- listParser(Some(reference))(innerContext).inBraces
      } yield Naming(variableName, assumption, substeps, finalInferenceApplication, reference)
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
              scopingStatement)(
              Seq(variableName)),
            innerProvenStatement.reference.add(reference))
        }
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
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
  object ScopedVariable {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[ScopedVariable] = {
      val scopingStatement = context.scopingStatement
        .getOrElse(throw new Exception("Scoped variable step could not find scoping statement"))
      for {
        variableName <- Parser.singleWord
        innerContext = context.addParameters(variableName)
        substeps <- listParser(Some(reference))(innerContext).inBraces
      } yield ScopedVariable(variableName, substeps, scopingStatement, reference)
    }
  }

  case class Target(statement: Statement, reference: Reference.Direct) extends Step {
    override def provenStatements = Seq(ProvenStatement(statement, reference))
    override def referencedInferenceIds = Set.empty
    override def referenceMap = ReferenceMap.empty
    override def length = 1
    override def intermediateReferences = Nil
    override def lastReference = Some(reference.value)
    override def getLines(
      referenceMap: ReferenceMap,
      indentLevel: Int,
      additionalReference: Option[String])(
      implicit displayContext: DisplayContext
    ): Seq[ProofLine] = {
      Seq(ProofLine(
        "Target:",
        ProofLine.Expression.create(statement, referenceMap.getReferrers(reference.value, additionalReference)),
        Some(reference.value),
        indentLevel,
        None))
    }
    override def isSingleAssertion = false
    def serializedLines = Seq(s"target ${statement.serialized}")
  }
  object Target {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[Target] = {
      for {
        statement <- Statement.parser
      } yield Target(statement, reference)
    }
  }

  implicit class StepSeqOps(steps: Seq[Step]) {
    def intermediateReferences: Seq[String] = {
      steps.dropRight(1).flatMap { step =>
        step.intermediateReferences ++ step.lastReference.toSeq
      } ++ steps.lastOption.toSeq.flatMap(_.intermediateReferences)
    }
  }


  def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Assumption.parser(reference)
      case "let" => Naming.parser(reference)
      case "assert" => Assertion.parser(reference)
      case "take" => ScopedVariable.parser(reference)
      case "target" => Target.parser(reference)
    }
  }
  def listParser(baseReference: Option[Reference.Direct])(implicit context: ParsingContext): Parser[Seq[Step]] = {
    Parser.whileDefined[Step] { (_, index) =>
      parser(Reference.nextReference(baseReference, index.toString))
    }
  }
}
