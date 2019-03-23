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
  def recalculateReferences(path: Seq[Int], stepContext: StepContext, parsingContext: ParsingContext): Option[Step]
  def referencedInferenceIds: Set[String]
  def length: Int
  def serializedLines: Seq[String]
}

case class StepContext(availableStatements: Seq[ProvenStatement], boundVariableLists: Seq[Seq[String]]) {
  def externalDepth: Int = boundVariableLists.length
  def addStatement(statement: ProvenStatement) = copy(availableStatements = availableStatements :+ statement)
  def addStatements(statements: Seq[ProvenStatement]) = copy(availableStatements = availableStatements ++ statements)
  def addBoundVariable(name: String) = copy(
    availableStatements = availableStatements.map(_.insertExternalParameters(1)),
    boundVariableLists = boundVariableLists :+ Seq(name))
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
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext, parsingContext: ParsingContext): Option[Step] = Some(this)
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
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext, parsingContext: ParsingContext): Option[Step] = {
      substeps
        .recalculateReferences(path, stepContext.addStatement(ProvenStatement(assumption, PreviousLineReference(path.mkString(".") + "a", Nil))), parsingContext)
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
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext, parsingContext: ParsingContext): Option[Step] = {
      substeps
        .recalculateReferences(path, stepContext.addStatement(ProvenStatement(assumption, PreviousLineReference(path.mkString(".") + "a", Nil))).addBoundVariable(variableName), parsingContext)
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
          .addBoundVariable(variableName)
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
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext, parsingContext: ParsingContext): Option[Step] = {
      substeps
        .recalculateReferences(path, stepContext.addBoundVariable(variableName), parsingContext)
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
        substeps <- listParser(path)(innerContext, stepContext.addBoundVariable(variableName)).inBraces
      } yield ScopedVariable(variableName, substeps, scopingStatement)
    }
  }

  case class Target(statement: Statement, context: StepContext) extends Step {
    val `type` = "target"
    override def provenStatement: Option[Statement] = Some(statement)
    override def findSubstep(index: Int, innerIndexes: Seq[Int]): Option[Step] = None
    override def replaceSubstep(index: Int, substepIndexes: Seq[Int], newStep: Step): Step = throw new Exception("Cannot replace substep in target")
    override def insertSubstep(index: Int, innerIndexes: Seq[Int], newStep: Step): Option[Step] = None
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext, parsingContext: ParsingContext): Option[Step] = Some(this)
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
      inference: Inference.Summary,
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
    override def recalculateReferences(path: Seq[Int], stepContext: StepContext, parsingContext: ParsingContext): Option[Step] = {
      premises.map(_.recalculateReferences(stepContext, parsingContext)).traverseOption.map(newPremises => copy(premises = newPremises))
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
      def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Option[Premise]
      def isIncomplete: Boolean
    }
    object Premise {
      sealed trait SingleLinePremise extends Premise {
        def referencedLine: PreviousLineReference
        override def referencedLines: Set[PreviousLineReference] = Set(referencedLine)
        override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Option[SingleLinePremise]
      }
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
        val `type` = "pending"
        override def serializedLines: Seq[String] = Seq(s"pending ${statement.serialized}")
        override def referencedInferenceIds: Set[String] = Set.empty
        override def referencedLines: Set[PreviousLineReference] = Set.empty
        override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], NewAssert.Premise.Pending] = Map(path -> this)
        override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Option[Premise] = {
          ProofHelper.findPremise(statement, stepContext, parsingContext).orElse(Some(this))
        }
        override def isIncomplete: Boolean = true
      }
      object Pending {
        def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Pending] = {
          Statement.parser.map(Pending(_))
        }
      }

      case class Given(statement: Statement, referencedLine: PreviousLineReference) extends Leaf with SingleLinePremise {
        val `type` = "given"
        override def serializedLines: Seq[String] = Seq("given")
        override def referencedInferenceIds: Set[String] = Set.empty
        override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], NewAssert.Premise.Pending] = Map.empty
        override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Option[SingleLinePremise] = {
          ProofHelper.findPremise(statement, stepContext, parsingContext)
        }
        override def isIncomplete: Boolean = false
      }
      object Given {
        def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Given] = {
          val provenStatement = stepContext.findProvenStatement(targetStatement).getOrElse(throw new Exception(s"Statement $targetStatement was not given"))
          Parser.constant(Given(provenStatement.statement, provenStatement.reference))
        }
      }

      case class Expansion(
          statement: Statement,
          inference: Inference.Summary,
          premises: Seq[NewAssert.Premise],
          substitutions: Substitutions)
        extends Premise
      {
        val `type` = "expansion"
        override def serializedLines: Seq[String] = Seq(s"expanded ${statement.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)}") ++
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
        override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Option[Premise] = {
          premises.map(_.recalculateReferences(stepContext, parsingContext)).traverseOption.map(newPremises => copy(premises = newPremises))
        }
        override def isIncomplete: Boolean = premises.exists(_.isIncomplete)
      }
      object Expansion {
        def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Expansion] = {
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
            Premise.Expansion(statement, inference, premises, substitutions)
          }
        }
      }

      case class Simplification(statement: Statement, premise: Premise.SingleLinePremise, inference: Inference.Summary, substitutions: Substitutions, path: Seq[Int]) extends SingleLinePremise {
        val `type` = "simplification"
        override def referencedLine: PreviousLineReference = premise.referencedLine.addPath(path)
        override def referencedInferenceIds: Set[String] = premise.referencedInferenceIds + inference.id
        override def serializedLines: Seq[String] = Seq(s"simplified ${statement.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)}") ++ premise.serializedLines.indent
        override def getPendingPremises(path: Seq[Int]): Map[Seq[Int], Pending] = premise.getPendingPremises(path :+ 0)
        override def updateAtPath(path: Seq[Int], f: Premise => Try[Premise]): Option[Try[Premise]] = path match {
          case Nil =>
            Some(f(this))
          case 0 +: tail =>
            premise.updateAtPath(tail, f)
          case _ =>
            None
        }
        override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Option[SingleLinePremise] = {
          premise.recalculateReferences(stepContext, parsingContext).map(newPremise => copy(premise = newPremise))
            .orElse(ProofHelper.findPremise(statement, stepContext, parsingContext))
        }
        override def isIncomplete: Boolean = premise.isIncomplete
      }
      object Simplification {
        def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Simplification] = {
          for {
            statement <- Statement.parser
            inference <- Inference.parser
            substitutions <- inference.substitutionsParser
            premiseStatements = inference.substitutePremisesAndValidateConclusion(substitutions, statement, parsingContext.parameterDepth)
            premiseStatement = premiseStatements.single.getOrElse(throw new Exception("Simplification inference must have a single premise"))
            premise <- premiseParser(premiseStatement)
          } yield {
            if (statement != targetStatement) {
              throw new Exception(s"Statement $statement did not match target $targetStatement")
            }
            val path = premiseStatement.findComponentPath(statement).getOrElse(throw new Exception(s"Could not find $statement in $premiseStatement"))
            val singleLinePremise = premise.asOptionalInstanceOf[SingleLinePremise].getOrElse(throw new Exception("Simplification premise must be single line"))
            Premise.Simplification(statement, singleLinePremise, inference, substitutions, path)
          }
        }
      }
    }

    def premiseParser(statement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Premise] = {
      Parser.selectWordParser("premise") {
        case "pending" =>
          Pending.parser(statement)
        case "given" =>
          Given.parser(statement)
        case "expanded" =>
          Expansion.parser(statement)
        case "simplified" =>
          Simplification.parser(statement)
      }
    }

    def premisesParser(statements: Seq[Statement])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Seq[Premise]] = {
      statements.foldLeft(Parser.constant(Seq.empty[Premise])) { (parserSoFar, statement) =>
        for {
          premisesSoFar <- parserSoFar
          newPremise <- premiseParser(statement)
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
    def recalculateReferences(path: Seq[Int], stepContext: StepContext, parsingContext: ParsingContext): Option[Seq[Step]] = {
      steps.zipWithIndex.mapFoldOption(stepContext) { case (currentContext, (oldStep, i)) =>
        for {
          newStep <- oldStep.recalculateReferences(path :+ i, currentContext, parsingContext)
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
