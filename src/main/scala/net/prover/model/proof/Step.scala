package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model._

sealed trait Step {
  def reference: Reference.Direct
  def provenStatements: Seq[ProvenStatement]
  def findStepWithContext(indexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)] = {
    indexes match {
      case Nil =>
        Some((this, currentContext))
      case head +: tail =>
        findSubstepWithContext(head, tail, currentContext)
    }
  }
  def findSubstepWithContext(index: Int, innerIndexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)]
  def replaceStep(substepIndexes: Seq[Int], newStep: Step): Step = {
    substepIndexes match {
      case Nil =>
        newStep
      case head +: tail =>
        replaceSubstep(head, tail, newStep)
    }
  }
  def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
  def length: Int
  def intermediateReferences: Seq[String]
  def lastReference: Option[String]
  def serializedLines: Seq[String]
}

case class StepContext(availableStatements: Seq[ProvenStatement], externalDepth: Int) {
  def addStatement(statement: ProvenStatement) = copy(availableStatements = availableStatements :+ statement)
  def addStatements(statements: Seq[ProvenStatement]) = copy(availableStatements = availableStatements ++ statements)
  def increaseExternalDepth() = copy(externalDepth = externalDepth + 1)
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
    override def findSubstepWithContext(index: Int, innerIndexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in assertion")
    override def length = 1
    override def intermediateReferences = Nil
    override def lastReference = Some(reference.value)
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
    override def findSubstepWithContext(index: Int, innerIndexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)] = {
      substeps.findSubstepWithContext(index, innerIndexes, currentContext.addStatement(ProvenStatement(assumption, reference.getChildForAssumption)))
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
    override def length = substeps.map(_.length).sum
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = substeps.lastOption.flatMap(_.lastReference)
    override def serializedLines = Seq(s"assume ${assumption.serialized} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Assumption {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[Assumption] = {
      val deductionStatement = context.deductionStatementOption
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
    override def findSubstepWithContext(index: Int, innerIndexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)] = {
      substeps.findSubstepWithContext(index, innerIndexes, currentContext.addStatement(ProvenStatement(assumption, assumptionReference)).increaseExternalDepth())
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def referencedInferenceIds = substeps.flatMap(_.referencedInferenceIds).toSet ++ finalInferenceApplication.referencedInferenceIds
    override def referenceMap = substeps.map(_.referenceMap).foldTogether ++
      ReferenceMap(finalAssertionReference.value -> finalInferenceApplication.lineReferences)
    override def length = substeps.map(_.length).sum + 1
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = Some(reference.value)
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
    override def findSubstepWithContext(index: Int, innerIndexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)] = {
      substeps.findSubstepWithContext(index, innerIndexes, currentContext.increaseExternalDepth())
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
    override def length = substeps.map(_.length).sum
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = substeps.lastOption.flatMap(_.lastReference)
    override def serializedLines = Seq(s"take $variableName {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object ScopedVariable {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[ScopedVariable] = {
      val scopingStatement = context.scopingStatementOption
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
    override def findSubstepWithContext(index: Int, innerIndexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in target")
    override def referencedInferenceIds = Set.empty
    override def referenceMap = ReferenceMap.empty
    override def length = 1
    override def intermediateReferences = Nil
    override def lastReference = Some(reference.value)
    def serializedLines = Seq(s"target ${statement.serialized}")
  }
  object Target {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[Target] = {
      for {
        statement <- Statement.parser
      } yield Target(statement, reference)
    }
  }

  case class NewAssert(
      statement: Statement,
      inference: Inference,
      premises: Seq[NewAssert.Premise],
      substitutions: Substitutions,
      reference: Reference.Direct)
    extends Step
  {
    override def provenStatements: Seq[ProvenStatement] = Seq(ProvenStatement(statement, reference))
    override def findSubstepWithContext(index: Int, innerIndexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in assertion")
    override def referencedInferenceIds: Set[String] = Set(inference.id)
    override def referenceMap: ReferenceMap = ReferenceMap(reference.value -> Set.empty[(String, Seq[Int])])
    override def length: Int = 1
    override def intermediateReferences: Seq[String] = Nil
    override def lastReference: Option[String] = Some(reference.value)
    override def serializedLines = {
      Seq(s"prove ${statement.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)}") ++
        premises.map(_.serialized).indent
    }

  }
  object NewAssert {
    sealed trait Premise {
      def serialized: String
    }
    case class FloatingPremise(statement: Statement) extends Premise {
      override def serialized: String = "?"
    }

    def premisesParser(statements: Seq[Statement]): Parser[Seq[Premise]] = {
      statements.foldLeft(Parser.constant(Seq.empty[Premise])) { (parserSoFar, statement) =>
        for {
          premisesSoFar <- parserSoFar
          newPremise <- Parser.selectWordParser("premise") {
            case "?" => Parser.constant(FloatingPremise(statement))
          }
        } yield premisesSoFar :+ newPremise
      }
    }
    def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[NewAssert] = {
      for {
        statement <- Statement.parser
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        premiseStatements = inference.substitutePremisesAndValidateConclusion(substitutions, statement, context.parameterDepth)
        premises <- premisesParser(premiseStatements)
      } yield NewAssert(statement, inference, premises, substitutions, reference)
    }
  }

  implicit class StepSeqOps(steps: Seq[Step]) {
    def intermediateReferences: Seq[String] = {
      steps.dropRight(1).flatMap { step =>
        step.intermediateReferences ++ step.lastReference.toSeq
      } ++ steps.lastOption.toSeq.flatMap(_.intermediateReferences)
    }
    def findSubstepWithContext(index: Int, innerIndexes: Seq[Int], currentContext: StepContext): Option[(Step, StepContext)] = {
      for {
        step <- steps.lift(index)
        previousSteps = steps.take(index)
        innerContext = currentContext.addStatements(previousSteps.flatMap(_.provenStatements))
        result <- step.findStepWithContext(innerIndexes, innerContext)
      } yield result
    }
  }

  def parser(reference: Reference.Direct)(implicit context: ParsingContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Assumption.parser(reference)
      case "let" => Naming.parser(reference)
      case "assert" => Assertion.parser(reference)
      case "take" => ScopedVariable.parser(reference)
      case "target" => Target.parser(reference)
      case "prove" => NewAssert.parser(reference)
    }
  }
  def listParser(baseReference: Option[Reference.Direct])(implicit context: ParsingContext): Parser[Seq[Step]] = {
    Parser.whileDefined[Step] { (_, index) =>
      parser(Reference.nextReference(baseReference, index.toString))
    }
  }
}
