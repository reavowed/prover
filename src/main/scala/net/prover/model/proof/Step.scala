package net.prover.model.proof

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.entries.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof.Step.NewAssert.Premise._

import scala.util.Try

@JsonIgnoreProperties(Array("context", "substitutions", "deductionStatement", "scopingStatement"))
sealed trait Step {
  @JsonSerialize
  def provenStatement: Option[Statement]
  def getSubstep(index: Int, outerContext: StepContext): Option[(Step, StepContext)]
  def extractSubstep(index: Int): Option[Option[(Step, Step)]]
  def modifySubsteps(outerContext: StepContext, f: (Seq[Step], StepContext) => Option[Seq[Step]]): Option[Step]
  def tryModifySubsteps(outerContext: StepContext, f: (Seq[Step], StepContext) => Option[Try[Seq[Step]]]): Option[Try[Step]]
  def tryModifySubstepsWithResult[T](f: Seq[Step] => Option[Try[(Seq[Step], T)]]): Option[Try[(Step, T)]]
  def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step]
  def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Step
  def referencedInferenceIds: Set[String]
  def referencedDefinitions: Set[ExpressionDefinition]
  @JsonSerialize
  def referencedLines: Set[PreviousLineReference]
  def length: Int
  def serializedLines: Seq[String]
}

case class StepContext(path: Seq[Int], availableStatements: Seq[ProvenStatement], boundVariableLists: Seq[Seq[String]]) {
  def externalDepth: Int = boundVariableLists.length
  def atIndex(index: Int): StepContext = copy(path = path :+ index)
  def addStatement(statement: ProvenStatement): StepContext = copy(availableStatements = availableStatements :+ statement)
  def addStatements(statements: Seq[ProvenStatement]): StepContext = copy(availableStatements = availableStatements ++ statements)
  def addStep(step: Step): StepContext = addSteps(Seq(step))
  def addSteps(steps: Seq[Step]): StepContext = {
    val provenStatements = steps
      .mapWithIndex((step, index) =>
        step.provenStatement.map(ProvenStatement(_, PreviousLineReference((path :+ index).mkString("."), Nil)))
    ).collectDefined
    addStatements(provenStatements)
  }
  def addBoundVariable(name: String): StepContext = copy(
    availableStatements = availableStatements.map(_.insertExternalParameters(1)),
    boundVariableLists = boundVariableLists :+ Seq(name))
  def findProvenStatement(statement: Statement): Option[ProvenStatement] = {
    availableStatements.find(_.statement == statement)
  }
}
object StepContext {
  def justWithPremises(premises: Seq[Statement]): StepContext = StepContext(Nil, premises.mapWithIndex((p, i) => ProvenStatement(p, PreviousLineReference(s"p$i", Nil))), Nil)
}

object Step {
  sealed trait WithoutSubsteps extends Step {
    override def getSubstep(index: Int, outerContext: StepContext): Option[(Step, StepContext)] = None
    override def extractSubstep(index: Int): Option[Option[(Step, Step)]] = None
    override def modifySubsteps(outerContext: StepContext, f: (Seq[Step], StepContext) => Option[Seq[Step]]): Option[Step] = None
    override def tryModifySubsteps(outerContext: StepContext, f: (Seq[Step], StepContext) => Option[Try[Seq[Step]]]): Option[Try[Step]] = None
    override def tryModifySubstepsWithResult[T](f: Seq[Step] => Option[Try[(Seq[Step], T)]]): Option[Try[(Step, T)]] = None
  }
  sealed trait WithSubsteps extends Step {
    def substeps: Seq[Step]
    def specifyContext(outerContext: StepContext): StepContext
    def replaceSubsteps(newSubsteps: Seq[Step]): Step
    def modifyStepForExtraction(step: Step): Option[Step]
    override def getSubstep(index: Int, outerContext: StepContext): Option[(Step, StepContext)] = {
      substeps.lift(index).map { step =>
        step -> specifyContext(outerContext.atIndex(index)).addSteps(substeps.take(index))
      }
    }
    override def extractSubstep(index: Int): Option[Option[(Step, Step)]] = {
      substeps.lift(index)
        .map(modifyStepForExtraction)
        .map(_.map(replaceSubsteps(substeps.removeAtIndex(index)) -> _))
    }
    override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Step = {
      val newSubsteps = substeps.recalculateReferences(specifyContext(stepContext), parsingContext)
      replaceSubsteps(newSubsteps)
    }
    override def modifySubsteps(outerContext: StepContext, f: (Seq[Step], StepContext) => Option[Seq[Step]]): Option[Step] = {
      f(substeps, specifyContext(outerContext)).map(replaceSubsteps)
    }
    override def tryModifySubsteps(outerContext: StepContext, f: (Seq[Step], StepContext) => Option[Try[Seq[Step]]]): Option[Try[Step]] = {
      f(substeps, specifyContext(outerContext)).map(_.map(replaceSubsteps))
    }
    override def tryModifySubstepsWithResult[T](f: Seq[Step] => Option[Try[(Seq[Step], T)]]): Option[Try[(Step, T)]] = f(substeps).map(_.map(_.mapLeft(replaceSubsteps)))
  }

  case class Deduction(
      assumption: Statement,
      substeps: Seq[Step],
      deductionStatement: StatementDefinition)
    extends Step.WithSubsteps
  {
    val `type` = "deduction"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => DefinedStatement(Seq(assumption, s), deductionStatement)(Nil))
    }
    override def specifyContext(outerContext: StepContext): StepContext = {
      outerContext.addStatement(ProvenStatement(assumption, PreviousLineReference(outerContext.path.mkString(".") + "a", Nil)))
    }
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def modifyStepForExtraction(step: Step): Option[Step] = Some(step)
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newAssumption <- assumption.removeExternalParameters(numberOfParametersToRemove)
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      } yield Deduction(newAssumption, newSubsteps, deductionStatement)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedLines: Set[PreviousLineReference] = substeps.flatMap(_.referencedLines).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = assumption.referencedDefinitions ++ substeps.flatMap(_.referencedDefinitions).toSet + deductionStatement
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(s"assume ${assumption.serialized} {") ++
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
      statement: Statement,
      substeps: Seq[Step],
      inference: Inference.Summary,
      premises: Seq[NewAssert.Premise],
      substitutions: Substitutions)
    extends Step.WithSubsteps
  {
    val `type` = "naming"
    override def provenStatement: Option[Statement] = Some(statement)
    override def specifyContext(outerContext: StepContext): StepContext = {
      outerContext.addBoundVariable(variableName).addStatement(ProvenStatement(assumption, PreviousLineReference(outerContext.path.mkString(".") + "a", Nil)))
    }
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def modifyStepForExtraction(step: Step): Option[Step] = step.removeExternalParameters(1)
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newAssumption <- assumption.removeExternalParameters(numberOfParametersToRemove)
        newStatement <- statement.removeExternalParameters(numberOfParametersToRemove)
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      } yield Naming(variableName, newAssumption, newStatement, newSubsteps, inference, premises, substitutions)
    }
    override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Step = {
      val newSubsteps = substeps.recalculateReferences(specifyContext(stepContext), parsingContext)
      val newPremises = premises.map(p => ProofHelper.findPremise(p.statement, stepContext, parsingContext))
      copy(substeps = newSubsteps, premises = newPremises)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet + inference.id
    override def referencedDefinitions: Set[ExpressionDefinition] = assumption.referencedDefinitions ++ substeps.flatMap(_.referencedDefinitions).toSet
    override def referencedLines: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def length: Int = substeps.map(_.length).sum + 1
    override def serializedLines: Seq[String] = Seq(s"let $variableName ${assumption.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)} {") ++
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
          .addBoundVariable(variableName)
          .addStatement(ProvenStatement(assumption, PreviousLineReference(path.mkString(".") + "a", Nil)))
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        substeps <- listParser(path)(innerParsingContext, innerStepContext).inBraces
      } yield {
        val internalConclusion = substeps.flatMap(_.provenStatement).lastOption.getOrElse(throw new Exception("No conclusion for naming step"))
        val extractedConclusion = internalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
        val scopingStatement = parsingContext.scopingStatementOption.getOrElse(throw new Exception("Naming step requires a scoping statement"))
        val deductionStatement = parsingContext.deductionStatementOption.getOrElse(throw new Exception("Naming step requires a deduction statement"))
        val internalPremise = DefinedStatement(Seq(DefinedStatement(Seq(assumption, internalConclusion), deductionStatement)(Nil)), scopingStatement)(Seq(variableName))
        val substitutedPremises = inference.substitutePremisesAndValidateConclusion(extractedConclusion, substitutions, stepContext.externalDepth)
        val availablePremises = ProofHelper.getAvailablePremises(stepContext, parsingContext)
        val premises = substitutedPremises match {
          case init :+ last =>
            if (last != internalPremise)
              throw new Exception(s"Inference premise $last did not match $internalPremise")
            init.map(s => availablePremises.find(_.statement == s).getOrElse(throw new Exception(s"Could not find premise $s")))
          case Nil =>
            throw new Exception("Naming step inference had no premises")
        }
        Naming(variableName, assumption, extractedConclusion, substeps, inference, premises, substitutions)
      }
    }
  }

  case class ScopedVariable(
      variableName: String,
      substeps: Seq[Step],
      scopingStatement: StatementDefinition)
    extends Step.WithSubsteps
  {
    val `type` = "scopedVariable"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => DefinedStatement(Seq(s), scopingStatement)(Seq(variableName)))
    }
    override def specifyContext(outerContext: StepContext): StepContext = {
      outerContext.addBoundVariable(variableName)
    }
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def modifyStepForExtraction(step: Step): Option[Step] = step.removeExternalParameters(1)
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      } yield ScopedVariable(variableName, newSubsteps, scopingStatement)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = substeps.flatMap(_.referencedDefinitions).toSet + scopingStatement
    override def referencedLines: Set[PreviousLineReference] = substeps.flatMap(_.referencedLines).toSet
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(s"take $variableName {") ++
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

  case class Target(statement: Statement) extends Step.WithoutSubsteps {
    val `type` = "target"
    override def provenStatement: Option[Statement] = Some(statement)
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        s <- statement.removeExternalParameters(numberOfParametersToRemove)
      } yield copy(statement = s)
    }
    override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Step = this
    override def referencedInferenceIds: Set[String] = Set.empty
    override def referencedDefinitions: Set[ExpressionDefinition] = statement.referencedDefinitions
    override def referencedLines: Set[PreviousLineReference] = Set.empty
    override def length = 1
    def serializedLines: Seq[String] = Seq(s"target ${statement.serialized}")
  }
  object Target {
    def parser(path: Seq[Int])(implicit context: ParsingContext, stepContext: StepContext): Parser[Target] = {
      for {
        statement <- Statement.parser
      } yield Target(statement)
    }
  }

  case class Elided(substeps: Seq[Step], highlightedInference: Option[Inference.Summary]) extends Step.WithSubsteps {
    val `type` = "elided"
    override def provenStatement: Option[Statement] = substeps.flatMap(_.provenStatement).lastOption
    override def specifyContext(outerContext: StepContext): StepContext = outerContext
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def modifyStepForExtraction(step: Step): Option[Step] = Some(step)
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      } yield Elided(newSubsteps, highlightedInference)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = substeps.flatMap(_.referencedDefinitions).toSet
    override def referencedLines: Set[PreviousLineReference] = substeps.flatMap(_.referencedLines).toSet
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(("elided" +: highlightedInference.map(_.id).toSeq :+ "{").mkString(" ")) ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Elided {
    def parser(path: Seq[Int])(implicit context: ParsingContext, stepContext: StepContext): Parser[Elided] = {
      for {
        highlightedInference <- Inference.parser.tryOrNone
        substeps <- listParser(path).inBraces
      } yield Elided(substeps, highlightedInference)
    }
  }

  case class NewAssert(
      statement: Statement,
      inference: Inference.Summary,
      premises: Seq[NewAssert.Premise],
      substitutions: Substitutions)
    extends Step.WithoutSubsteps
  {
    val `type`: String = "assertion"
    override def provenStatement: Option[Statement] = Some(statement)
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newStatement <- statement.removeExternalParameters(numberOfParametersToRemove)
        newPremises <- premises.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
        newSubstitutions <- substitutions.removeExternalParameters(numberOfParametersToRemove)
      } yield NewAssert(newStatement, inference, newPremises, newSubstitutions)
    }
    override def recalculateReferences(stepContext: StepContext, parsingContext: ParsingContext): Step = {
      val newPremises = premises.map(p => ProofHelper.findPremise(p.statement, stepContext, parsingContext))
      copy(premises = newPremises)
    }
    override def referencedInferenceIds: Set[String] = Set(inference.id) ++ premises.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = statement.referencedDefinitions
    override def referencedLines: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
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
          premises.tryUpdateAtIndexIfDefined(head, _.updateAtPath(tail, f)).mapMap(newPremises => copy(premises = newPremises))
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
      def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise]
      def isIncomplete: Boolean
    }
    object Premise {
      sealed trait SingleLinePremise extends Premise {
        def referencedLine: PreviousLineReference
        override def referencedLines: Set[PreviousLineReference] = Set(referencedLine)
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
        override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise] = {
          statement.removeExternalParameters(numberOfParametersToRemove).map(Pending(_))
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
        override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise] = {
          statement.removeExternalParameters(numberOfParametersToRemove).map(Pending(_))
        }
        override def isIncomplete: Boolean = false
      }
      object Given {
        def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Given] = {
          val provenStatement = stepContext.findProvenStatement(targetStatement).getOrElse(throw new Exception(s"Statement $targetStatement was not given"))
          Parser.constant(Given(provenStatement.statement, provenStatement.reference))
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
        override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Premise] = {
          statement.removeExternalParameters(numberOfParametersToRemove).map(Pending(_))
        }
        override def isIncomplete: Boolean = premise.isIncomplete
      }
      object Simplification {
        def parser(targetStatement: Statement)(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Simplification] = {
          for {
            statement <- Statement.parser
            inference <- Inference.parser
            substitutions <- inference.substitutionsParser
            premiseStatements = inference.substitutePremisesAndValidateConclusion(statement, substitutions, parsingContext.parameterDepth)
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
        premiseStatements = inference.substitutePremisesAndValidateConclusion(statement, substitutions, parsingContext.parameterDepth)
        premises <- premisesParser(premiseStatements)
      } yield NewAssert(statement, inference, premises, substitutions)
    }
  }

  implicit class StepSeqOps(steps: Seq[Step]) {
    def recalculateReferences(outerContext: StepContext, parsingContext: ParsingContext): Seq[Step] = {
      steps.zipWithIndex.mapReduceWithPrevious[Step] { case (previousSteps, (oldStep, i)) =>
        oldStep.recalculateReferences(outerContext.addSteps(previousSteps).atIndex(i), parsingContext)
      }
    }
  }

  def parser(path: Seq[Int])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Deduction.parser(path)
      case "let" => Naming.parser(path)
      case "take" => ScopedVariable.parser(path)
      case "target" => Target.parser(path)
      case "prove" => NewAssert.parser(path)
      case "elided" => Elided.parser(path)
    }
  }
  def listParser(path: Seq[Int])(implicit parsingContext: ParsingContext, stepContext: StepContext): Parser[Seq[Step]] = {
    Parser.whileDefined[Step] { (previousSteps, index) =>
      val previousProvenStatements = previousSteps.mapWithIndex((s, i) => s.provenStatement.map(s => ProvenStatement(s, PreviousLineReference((path :+ i).mkString("."), Nil)))).collectDefined
      parser(path :+ index)(parsingContext, stepContext.addStatements(previousProvenStatements))
    }
  }
}
