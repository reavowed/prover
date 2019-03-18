package net.prover.model.proof

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof.Step.NewAssert.Premise._

import scala.util.Try

sealed trait Step {
  def reference: Reference.Direct
  def provenStatement: Option[ProvenStatement]
  def findStep(indexes: Seq[Int]): Option[Step] = {
    indexes match {
      case Nil =>
        Some(this)
      case head +: tail =>
        findSubstep(head, tail)
    }
  }
  def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step]
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
  def referencedLines: Set[PreviousLineReference]
  def length: Int
  def intermediateReferences: Seq[String]
  def lastReference: Option[String]
  def serializedLines: Seq[String]
}

case class StepContext(availableStatements: Seq[ProvenStatement], externalDepth: Int) {
  def addStatement(statement: ProvenStatement) = copy(availableStatements = availableStatements :+ statement)
  def addStatements(statements: Seq[ProvenStatement]) = copy(availableStatements = availableStatements ++ statements)
  def increaseExternalDepth() = copy(externalDepth = externalDepth + 1)
  def findProvenStatement(statement: Statement): Option[ProvenStatement] = {
    availableStatements.find(_.statement == statement)
  }
}

object Step {
  case class Assertion(
      assertion: Statement,
      inferenceApplication: InferenceApplication,
      reference: Reference.Direct)
    extends Step
  {
    override def provenStatement = Some(ProvenStatement(assertion, PreviousLineReference(reference.value, Nil)))
    override def referencedInferenceIds = inferenceApplication.referencedInferenceIds
    override def referencedLines: Set[PreviousLineReference] = inferenceApplication.referencedLines
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = None
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
    override def provenStatement: Option[ProvenStatement] = substeps.lastOption.flatMap(_.provenStatement).map { innerProvenStatement =>
      ProvenStatement(
        DefinedStatement(Seq(assumption, innerProvenStatement.statement), deductionStatement)(Nil),
        PreviousLineReference(reference.value, Nil))
    }
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = {
      substeps.findSubstep(index, innerIndexes)
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedLines: Set[PreviousLineReference] = substeps.flatMap(_.referencedLines).toSet
    override def length = substeps.map(_.length).sum
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = substeps.lastOption.flatMap(_.lastReference)
    override def serializedLines = Seq(s"assume ${assumption.serialized} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Assumption {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Assumption] = {
      val deductionStatement = parsingContext.deductionStatementOption
        .getOrElse(throw new Exception("Cannot prove a deduction without an appropriate statement definition"))
      for {
        assumption <- Statement.parser
        substeps <- listParser(Some(reference))(parsingContext, stepContext.addStatement(ProvenStatement(assumption, PreviousLineReference(reference.getChildForAssumption.value, Nil)))).inBraces
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

    override def provenStatement: Option[ProvenStatement] = Some(ProvenStatement(finalAssertion, PreviousLineReference(reference.value, Nil)))
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = {
      substeps.findSubstep(index, innerIndexes)
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def referencedInferenceIds = substeps.flatMap(_.referencedInferenceIds).toSet ++ finalInferenceApplication.referencedInferenceIds
    override def referencedLines: Set[PreviousLineReference] = substeps.flatMap(_.referencedLines).toSet ++ finalInferenceApplication.referencedLines
    override def length = substeps.map(_.length).sum + 1
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = Some(reference.value)
    override def serializedLines = Seq(s"let $variableName ${assumption.serialized} ${finalInferenceApplication.serialized} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }

  object Naming {
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Naming] = {
      for {
        variableName <- Parser.singleWord
        innerParsingContext = parsingContext.addParameters(variableName)
        assumption <- Statement.parser(innerParsingContext)
        innerStepContext = stepContext
          .addStatement(ProvenStatement(assumption, PreviousLineReference(reference.getChildForAssumption.value, Nil)))
          .increaseExternalDepth()
        finalInferenceApplication <- InferenceApplication.parser
        substeps <- listParser(Some(reference))(innerParsingContext, innerStepContext).inBraces
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
    override def provenStatement: Option[ProvenStatement] = {
      substeps.lastOption.flatMap(_.provenStatement).map { innerProvenStatement =>
        ProvenStatement(
          DefinedStatement(Seq(innerProvenStatement.statement), scopingStatement)(Seq(variableName)),
          PreviousLineReference(reference.value, Nil))
      }
    }
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = {
      substeps.findSubstep(index, innerIndexes)
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedLines: Set[PreviousLineReference] = substeps.flatMap(_.referencedLines).toSet
    override def length = substeps.map(_.length).sum
    override def intermediateReferences = substeps.intermediateReferences
    override def lastReference = substeps.lastOption.flatMap(_.lastReference)
    override def serializedLines = Seq(s"take $variableName {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object ScopedVariable {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext, stepContext: StepContext): Parser[ScopedVariable] = {
      val scopingStatement = context.scopingStatementOption
        .getOrElse(throw new Exception("Scoped variable step could not find scoping statement"))
      for {
        variableName <- Parser.singleWord
        innerContext = context.addParameters(variableName)
        substeps <- listParser(Some(reference))(innerContext, stepContext.increaseExternalDepth()).inBraces
      } yield ScopedVariable(variableName, substeps, scopingStatement, reference)
    }
  }

  case class Target(statement: Statement, reference: Reference.Direct, context: StepContext) extends Step {
    override def provenStatement: Option[ProvenStatement] = Some(ProvenStatement(statement, PreviousLineReference(reference.value, Nil)))
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in target")
    override def referencedInferenceIds = Set.empty
    override def referencedLines: Set[PreviousLineReference] = Set.empty
    override def length = 1
    override def intermediateReferences = Nil
    override def lastReference = Some(reference.value)
    def serializedLines = Seq(s"target ${statement.serialized}")
  }
  object Target {
    def parser(reference: Reference.Direct)(implicit context: ParsingContext, stepContext: StepContext): Parser[Target] = {
      for {
        statement <- Statement.parser
      } yield Target(statement, reference, stepContext)
    }
  }

  case class NewAssert(
      statement: Statement,
      inference: Inference,
      premises: Seq[NewAssert.Premise],
      substitutions: Substitutions,
      reference: Reference.Direct,
      context: StepContext)
    extends Step
  {
    override def provenStatement: Option[ProvenStatement] = Some(ProvenStatement(statement, PreviousLineReference(reference.value, Nil)))
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in assertion")
    override def referencedInferenceIds: Set[String] = Set(inference.id) ++ premises.flatMap(_.referencedInferenceIds).toSet
    override def referencedLines: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def length: Int = 1
    override def intermediateReferences: Seq[String] = Nil
    override def lastReference: Option[String] = Some(reference.value)
    override def serializedLines: Seq[String] = {
      Seq(s"prove ${statement.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)}") ++
        premises.flatMap(_.serializedLines).indent
    }
    def pendingPremises: Seq[(NewAssert.Premise.Pending, Seq[Int])] = {
      premises.flatMapWithIndex((p, i) => p.getPendingPremises(Seq(i)))
    }
    def tryUpdatePremiseAtPath(path: Seq[Int], f: NewAssert.Premise => Try[NewAssert.Premise]): Option[Try[NewAssert]] = {
      path match {
        case head +: tail =>
          premises.tryUpdateAtIndexIfDefined(path.head)(p => p.updateAtPath(path.tail, f)).map(_.map(newPremises => copy(premises = newPremises)))
        case Nil =>
          None
      }
    }
  }
  object NewAssert {
    sealed trait Premise {
      def statement: Statement
      def referencedInferenceIds: Set[String]
      def referencedLines: Set[PreviousLineReference]
      def serializedLines: Seq[String]
      def getPendingPremises(path: Seq[Int]): Seq[(NewAssert.Premise.Pending, Seq[Int])]
      def updateAtPath(path: Seq[Int], f: Premise => Try[Premise]): Option[Try[Premise]]
    }
    object Premise {
      sealed trait Leaf extends Premise {
        override def updateAtPath(path: Seq[Int], f: Premise => Try[Premise]): Option[Try[Premise]] = {
          path match {
            case Nil =>
              Some(f(this))
            case _ =>
              None
          }
        }
      }
      case class Pending(statement: Statement) extends Leaf {
        override def serializedLines: Seq[String] = Seq(s"pending ${statement.serialized}")
        override def referencedInferenceIds: Set[String] = Set.empty
        override def referencedLines: Set[PreviousLineReference] = Set.empty
        override def getPendingPremises(path: Seq[Int]): Seq[(NewAssert.Premise.Pending, Seq[Int])] = Seq((this, path))
      }
      object Pending {
        def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Pending] = {
          Statement.parser.map(Pending(_))
        }
      }

      case class Given(statement: Statement, reference: PreviousLineReference) extends Leaf {
        override def serializedLines: Seq[String] = Seq("given")
        override def referencedInferenceIds: Set[String] = Set.empty
        override def referencedLines: Set[PreviousLineReference] = Set(reference)
        override def getPendingPremises(path: Seq[Int]): Seq[(NewAssert.Premise.Pending, Seq[Int])] = Nil
      }
      object Given {
        def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Given] = {
          val provenStatement = stepContext.findProvenStatement(targetStatement).getOrElse(throw new Exception(s"Statement $targetStatement was not given"))
          Parser.constant(Given(provenStatement.statement, provenStatement.reference))
        }
      }

      case class Rearrangement(
          statement: Statement,
          inference: Inference,
          premises: Seq[NewAssert.Premise],
          substitutions: Substitutions)
        extends Premise
      {
        override def serializedLines: Seq[String] = Seq(s"rearranged ${statement.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)}") ++
          premises.flatMap(_.serializedLines).indent
        override def referencedInferenceIds: Set[String] = Set.empty
        override def referencedLines: Set[PreviousLineReference] = Set.empty
        override def getPendingPremises(path: Seq[Int]): Seq[(Pending, Seq[Int])] = premises.flatMapWithIndex((p, i) => p.getPendingPremises(path :+ i))
        override def updateAtPath(path: Seq[Int], f: Premise => Try[Premise]): Option[Try[Premise]] = {
          path match {
            case Nil =>
              Some(f(this))
            case _ =>
              premises.tryUpdateAtIndexIfDefined(path.head)(p => p.updateAtPath(path.tail, f)).map(_.map(newPremises => copy(premises = newPremises)))
          }
        }
      }
    }
    object Rearrangement {
      def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Rearrangement] = {
        for {
          statement <- Statement.parser
          inference <- Inference.parser
          substitutions <- inference.substitutionsParser
          premiseStatements = inference.substitutePremisesAndValidateConclusion(substitutions, statement, parsingContext.parameterDepth)
          premises <- premisesParser(premiseStatements)
        } yield {
          if (statement != targetStatement) {
            throw new Exception(s"Statement $statement did not match target $targetStatement")
          }
          Premise.Rearrangement(statement, inference, premises, substitutions)
        }
      }
    }

    def premisesParser(statements: Seq[Statement])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Seq[Premise]] = {
      statements.foldLeft(Parser.constant(Seq.empty[Premise])) { (parserSoFar, statement) =>
        for {
          premisesSoFar <- parserSoFar
          newPremise <- Parser.selectWordParser("premise") {
            case "pending" =>
              Pending.parser(statement)
            case "given" =>
              Given.parser(statement)
            case "rearranged" =>
              Rearrangement.parser(statement)
          }
        } yield premisesSoFar :+ newPremise
      }
    }
    def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[NewAssert] = {
      for {
        statement <- Statement.parser
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        premiseStatements = inference.substitutePremisesAndValidateConclusion(substitutions, statement, parsingContext.parameterDepth)
        premises <- premisesParser(premiseStatements)
      } yield NewAssert(statement, inference, premises, substitutions, reference, stepContext)
    }
  }

  implicit class StepSeqOps(steps: Seq[Step]) {
    def intermediateReferences: Seq[String] = {
      steps.dropRight(1).flatMap { step =>
        step.intermediateReferences ++ step.lastReference.toSeq
      } ++ steps.lastOption.toSeq.flatMap(_.intermediateReferences)
    }
    def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = {
      for {
        step <- steps.lift(index)
        previousSteps = steps.take(index)
        result <- step.findStep(innerIndexes)
      } yield result
    }
  }

  def parser(reference: Reference.Direct)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Assumption.parser(reference)
      case "let" => Naming.parser(reference)
      case "assert" => Assertion.parser(reference)
      case "take" => ScopedVariable.parser(reference)
      case "target" => Target.parser(reference)
      case "prove" => NewAssert.parser(reference)
    }
  }
  def listParser(baseReference: Option[Reference.Direct])(implicit context: ParsingContext, stepContext: StepContext): Parser[Seq[Step]] = {
    Parser.whileDefined[Step] { (_, index) =>
      parser(Reference.nextReference(baseReference, index.toString))
    }
  }
}
