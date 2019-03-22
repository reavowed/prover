package net.prover.model.proof

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof.Step.NewAssert.Premise._

import scala.util.Try

@JsonIgnoreProperties(Array("context", "substitutions", "deductionStatement", "scopingStatement"))
sealed trait Step {
  def provenStatement: Option[Statement]
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
  def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Step]
  def recalculateReferences(path: Seq[Int], stepContext: StepContext): Option[Step]
  def referencedInferenceIds: Set[String]
  def length: Int
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
      statement: Statement,
      inferenceApplication: InferenceApplication)
    extends Step
  {
    val `type` = "oldAssertion"
    val referencedLines: Set[PreviousLineReference] = inferenceApplication.referencedLines
    override def provenStatement: Option[Statement] = Some(statement)
    override def referencedInferenceIds = inferenceApplication.referencedInferenceIds
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in assertion")
    override def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Step] = None
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext): Option[Step] = Some(this)
    override def length = 1
    override def serializedLines = Seq(s"assert ${statement.serialized} ${inferenceApplication.serialized}")
  }
  object Assertion {
    def parser(path: Seq[Int])(implicit context: ParsingContext): Parser[Assertion] = {
      for {
        assertion <- Statement.parser
        inferenceApplication <- InferenceApplication.parser
      } yield Assertion(assertion, inferenceApplication)
    }
  }

  case class Deduction(
      assumption: Statement,
      substeps: Seq[Step],
      deductionStatement: StatementDefinition)
    extends Step
  {
    val `type` = "deduction"
    @JsonSerialize
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => DefinedStatement(Seq(assumption, s), deductionStatement)(Nil))
    }
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = {
      substeps.findSubstep(index, innerIndexes)
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Step] = {
      substeps.insertSubstep(index, innerIndexes, newStep).map(newSubsteps => copy(substeps = newSubsteps))
    }
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext): Option[Step] = {
      substeps
        .recalculateReferences(path, stepContext.addStatement(ProvenStatement(assumption, PreviousLineReference(path.mkString(".") + "a", Nil))))
        .map(newSubsteps => copy(substeps = newSubsteps))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def length = substeps.map(_.length).sum
    override def serializedLines = Seq(s"assume ${assumption.serialized} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Deduction {
    def parser(path: Seq[Int])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Deduction] = {
      val deductionStatement = parsingContext.deductionStatementOption
        .getOrElse(throw new Exception("Cannot prove a deduction without an appropriate statement definition"))
      for {
        assumption <- Statement.parser
        substeps <- listParser(path)(parsingContext, stepContext.addStatement(ProvenStatement(assumption, PreviousLineReference(path.mkString(".") + "a", Nil)))).inBraces
      } yield Deduction(assumption, substeps, deductionStatement)
    }
  }

  case class Naming(
      variableName: String,
      assumption: Statement,
      substeps: Seq[Step],
      finalInferenceApplication: InferenceApplication)
    extends Step
  {
    val `type` = "naming"
    override def provenStatement: Option[Statement] = {
      Some(finalInferenceApplication.conclusion)
    }
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = {
      substeps.findSubstep(index, innerIndexes)
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Step] = {
      substeps.insertSubstep(index, innerIndexes, newStep).map(newSubsteps => copy(substeps = newSubsteps))
    }
    // TODO: apply to inference application
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext): Option[Step] = {
      substeps
        .recalculateReferences(path, stepContext.addStatement(ProvenStatement(assumption, PreviousLineReference(path.mkString(".") + "a", Nil))).increaseExternalDepth())
        .map(newSubsteps => copy(substeps = newSubsteps))
    }
    override def referencedInferenceIds = substeps.flatMap(_.referencedInferenceIds).toSet ++ finalInferenceApplication.referencedInferenceIds
    override def length = substeps.map(_.length).sum + 1
    override def serializedLines = Seq(s"let $variableName ${assumption.serialized} ${finalInferenceApplication.serialized} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }

  object Naming {
    def parser(path: Seq[Int])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Naming] = {
      for {
        variableName <- Parser.singleWord
        innerParsingContext = parsingContext.addParameters(variableName)
        assumption <- Statement.parser(innerParsingContext)
        innerStepContext = stepContext
          .addStatement(ProvenStatement(assumption, PreviousLineReference(path.mkString(".") + "a", Nil)))
          .increaseExternalDepth()
        finalInferenceApplication <- InferenceApplication.parser
        substeps <- listParser(path)(innerParsingContext, innerStepContext).inBraces
      } yield Naming(variableName, assumption, substeps, finalInferenceApplication)
    }
  }

  case class ScopedVariable(
      variableName: String,
      substeps: Seq[Step],
      scopingStatement: StatementDefinition)
    extends Step
  {
    val `type` = "scopedVariable"
    @JsonSerialize
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => DefinedStatement(Seq(s), scopingStatement)(Seq(variableName)))
    }
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = {
      substeps.findSubstep(index, innerIndexes)
    }
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = {
      copy(substeps = substeps.updated(index, substeps(index).replaceStep(substepIndexes, newStep)))
    }
    override def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Step] = {
      substeps.insertSubstep(index, innerIndexes, newStep).map(newSubsteps => copy(substeps = newSubsteps))
    }
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext): Option[Step] = {
      substeps
        .recalculateReferences(path, stepContext.increaseExternalDepth())
        .map(newSubsteps => copy(substeps = newSubsteps))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def length = substeps.map(_.length).sum
    override def serializedLines = Seq(s"take $variableName {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object ScopedVariable {
    def parser(path: Seq[Int])(implicit context: ParsingContext, stepContext: StepContext): Parser[ScopedVariable] = {
      val scopingStatement = context.scopingStatementOption
        .getOrElse(throw new Exception("Scoped variable step could not find scoping statement"))
      for {
        variableName <- Parser.singleWord
        innerContext = context.addParameters(variableName)
        substeps <- listParser(path)(innerContext, stepContext.increaseExternalDepth()).inBraces
      } yield ScopedVariable(variableName, substeps, scopingStatement)
    }
  }

  case class Target(statement: Statement, context: StepContext) extends Step {
    val `type` = "target"
    override def provenStatement: Option[Statement] = Some(statement)
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in target")
    override def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Step] = None
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext): Option[Step] = Some(this)
    override def referencedInferenceIds = Set.empty
    override def length = 1
    def serializedLines = Seq(s"target ${statement.serialized}")
  }
  object Target {
    def parser(path: Seq[Int])(implicit context: ParsingContext, stepContext: StepContext): Parser[Target] = {
      for {
        statement <- Statement.parser
      } yield Target(statement, stepContext)
    }
  }

  case class NewAssert(
      statement: Statement,
      inference: Inference,
      premises: Seq[NewAssert.Premise],
      substitutions: Substitutions,
      context: StepContext)
    extends Step
  {
    val `type`: String = "assertion"
    val referencedLines: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def provenStatement: Option[Statement] = Some(statement)
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in assertion")
    override def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Step] = None
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext): Option[Step] = {
      premises.map(_.recalculateReferences(stepContext)).traverseOption.map(newPremises => copy(premises = newPremises))
    }
    override def referencedInferenceIds: Set[String] = Set(inference.id) ++ premises.flatMap(_.referencedInferenceIds).toSet
    override def length: Int = 1
    override def serializedLines: Seq[String] = {
      Seq(s"prove ${statement.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)}") ++
        premises.flatMap(_.serializedLines).indent
    }
    def pendingPremises: Map[Seq[Int], NewAssert.Premise.Pending] = {
      premises.flatMapWithIndex((p, i) => p.getPendingPremises(Seq(i)).toSeq).toMap
    }
    def tryUpdatePremiseAtPath(path: Seq[Int], f: NewAssert.Premise => Try[NewAssert.Premise]): Option[Try[NewAssert]] = {
      path match {
        case head +: tail =>
          premises.tryUpdateAtIndexIfDefined(path.head)(p => p.updateAtPath(path.tail, f)).map(_.map(newPremises => copy(premises = newPremises)))
        case Nil =>
          None
      }
    }
    def isIncomplete: Boolean = premises.exists(_.isIncomplete)
  }
  object NewAssert {
    sealed trait Premise {
      def statement: Statement
      def referencedInferenceIds: Set[String]
      def referencedLines: Set[PreviousLineReference]
      def serializedLines: Seq[String]
      def getPendingPremises(path: Seq[Int]): Map[Seq[Int], NewAssert.Premise.Pending]
      def updateAtPath(path: Seq[Int], f: Premise => Try[Premise]): Option[Try[Premise]]
      def recalculateReferences(stepContext: StepContext): Option[Premise]
      def isIncomplete: Boolean
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
        override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], NewAssert.Premise.Pending] = Map(path -> this)
        override def recalculateReferences(stepContext: StepContext): Option[Premise] = {
          stepContext.findProvenStatement(statement) match {
            case Some(ProvenStatement(provenStatement, reference)) =>
              Some(Given(provenStatement, reference))
            case None =>
              Some(this)
          }
        }
        override def isIncomplete: Boolean = true
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
        override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], NewAssert.Premise.Pending] = Map.empty
        override def recalculateReferences(stepContext: StepContext): Option[Premise] = {
          stepContext.findProvenStatement(statement) match {
            case Some(ProvenStatement(provenStatement, newReference)) =>
              Some(Given(provenStatement, newReference))
            case None =>
              None
          }
        }
        override def isIncomplete: Boolean = false
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
        override def referencedInferenceIds: Set[String] = premises.flatMap(_.referencedInferenceIds).toSet + inference.id
        override def referencedLines: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
        override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], NewAssert.Premise.Pending] = premises.flatMapWithIndex((p, i) => p.getPendingPremises(path :+ i).toSeq).toMap
        override def updateAtPath(path: Seq[Int], f: Premise => Try[Premise]): Option[Try[Premise]] = {
          path match {
            case Nil =>
              Some(f(this))
            case _ =>
              premises.tryUpdateAtIndexIfDefined(path.head)(p => p.updateAtPath(path.tail, f)).map(_.map(newPremises => copy(premises = newPremises)))
          }
        }
        override def recalculateReferences(stepContext: StepContext): Option[Premise] = {
          premises.map(_.recalculateReferences(stepContext)).traverseOption.map(newPremises => copy(premises = newPremises))
        }
        override def isIncomplete: Boolean = premises.exists(_.isIncomplete)
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
    def parser(path: Seq[Int])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[NewAssert] = {
      for {
        statement <- Statement.parser
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        premiseStatements = inference.substitutePremisesAndValidateConclusion(substitutions, statement, parsingContext.parameterDepth)
        premises <- premisesParser(premiseStatements)
      } yield NewAssert(statement, inference, premises, substitutions, stepContext)
    }
  }

  implicit class StepSeqOps(steps: Seq[Step]) {
    def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = {
      for {
        step <- steps.lift(index)
        previousSteps = steps.take(index)
        result <- step.findStep(innerIndexes)
      } yield result
    }
    def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Seq[Step]] = {
      innerIndexes match {
        case Nil =>
          val (before, after) = steps.splitAt(index)
          Some((before :+ newStep) ++ after)
        case head +: tail =>
          steps.updateAtIndexIfDefined(index)(_.insertSubstep(head, tail, newStep))
      }
    }
    def recalculateReferences(path: Seq[Int], stepContext: StepContext): Option[Seq[Step]] = {
      steps.zipWithIndex.mapFoldOption(stepContext) { case (currentContext, (oldStep, i)) =>
        for {
          newStep <- oldStep.recalculateReferences(path :+ i, currentContext)
        } yield {
          val newProvenStatements = newStep.provenStatement.toSeq.map(s => ProvenStatement(s, PreviousLineReference((path :+ i).mkString("."), Nil)))
          (currentContext.addStatements(newProvenStatements), newStep)
        }
      }.map(_._2)
    }
  }

  def parser(path: Seq[Int])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Deduction.parser(path)
      case "let" => Naming.parser(path)
      case "assert" => Assertion.parser(path)
      case "take" => ScopedVariable.parser(path)
      case "target" => Target.parser(path)
      case "prove" => NewAssert.parser(path)
    }
  }
  def listParser(path: Seq[Int])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Seq[Step]] = {
    Parser.whileDefined[Step] { (previousSteps, index) =>
      val previousProvenStatements = previousSteps.mapWithIndex((s, i) => s.provenStatement.map(s => ProvenStatement(s, PreviousLineReference((path :+ i).mkString("."), Nil)))).collectDefined
      parser(path :+ index)(parsingContext, stepContext.addStatements(previousProvenStatements))
    }
  }
}
