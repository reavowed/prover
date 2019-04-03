package net.prover.model.proof

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.entries.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{DefinedStatement, Statement}

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
  def insertExternalParameters(numberOfParametersToRemove: Int): Step
  def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step]
  def recalculateReferences(stepContext: StepContext, entryContext: EntryContext): Step
  def referencedInferenceIds: Set[String]
  def referencedDefinitions: Set[ExpressionDefinition]
  @JsonSerialize
  def referencedLines: Set[PreviousLineReference]
  def length: Int
  def serializedLines: Seq[String]
}

case class StepContext(stepReference: StepReference, availableStatements: Seq[ProvenStatement], boundVariableLists: Seq[Seq[String]]) {
  def externalDepth: Int = boundVariableLists.length
  def atIndex(index: Int): StepContext = copy(stepReference = stepReference.forChild(index))
  def addStatement(statement: ProvenStatement): StepContext = copy(availableStatements = availableStatements :+ statement)
  def addStatements(statements: Seq[ProvenStatement]): StepContext = copy(availableStatements = availableStatements ++ statements)
  def addStep(step: Step): StepContext = addSteps(Seq(step))
  def addSteps(steps: Seq[Step]): StepContext = {
    val provenStatements = steps.mapWithIndex((step, index) =>
      step.provenStatement.map(ProvenStatement(_, stepReference.forChild(index)))
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
  def justWithPremises(premises: Seq[Statement]): StepContext = StepContext(StepReference(Nil), premises.mapWithIndex((p, i) => ProvenStatement(p, PremiseReference(i))), Nil)
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
    def modifyStepForInsertion(step: Step): Step
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
    override def recalculateReferences(stepContext: StepContext, entryContext: EntryContext): Step = {
      val newSubsteps = substeps.recalculateReferences(specifyContext(stepContext), entryContext)
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
  sealed trait WithVariable extends Step.WithSubsteps {
    def variableName: String
    def replaceVariableName(newVariableName: String): Step
    override def modifyStepForInsertion(step: Step): Step = step.insertExternalParameters(1)
    override def modifyStepForExtraction(step: Step): Option[Step] = step.removeExternalParameters(1)
  }
  sealed trait WithoutVariable extends Step.WithSubsteps {
    override def modifyStepForInsertion(step: Step): Step = step
    override def modifyStepForExtraction(step: Step): Option[Step] = Some(step)
  }
  sealed trait WithTopLevelStatement extends Step {
    def updateStatement(f: Statement => Try[Statement]): Try[Step]
  }

  case class Deduction(
      assumption: Statement,
      substeps: Seq[Step],
      deductionStatement: StatementDefinition)
    extends Step.WithSubsteps with WithoutVariable with WithTopLevelStatement
  {
    val `type` = "deduction"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => DefinedStatement(Seq(assumption, s), deductionStatement)(Nil))
    }
    override def specifyContext(outerContext: StepContext): StepContext = {
      outerContext.addStatement(ProvenStatement(assumption, outerContext.stepReference.withSuffix("a")))
    }
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(assumption).map(a => copy(assumption = a))
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      Deduction(
        assumption.insertExternalParameters(numberOfParametersToInsert),
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert)),
        deductionStatement)
    }
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
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Deduction] = {
      val deductionStatement = entryContext.deductionStatementOption
        .getOrElse(throw new Exception("Cannot prove a deduction without an appropriate statement definition"))
      for {
        assumption <- Statement.parser
        substeps <- listParser(entryContext, stepContext.addStatement(ProvenStatement(assumption, stepContext.stepReference.withSuffix("a")))).inBraces
      } yield Deduction(assumption, substeps, deductionStatement)
    }
  }

  case class Naming(
      variableName: String,
      assumption: Statement,
      statement: Statement,
      substeps: Seq[Step],
      inference: Inference.Summary,
      premises: Seq[Premise],
      substitutions: Substitutions)
    extends Step.WithSubsteps with WithVariable with WithTopLevelStatement
  {
    val `type` = "naming"
    override def provenStatement: Option[Statement] = Some(statement)
    override def specifyContext(outerContext: StepContext): StepContext = {
      outerContext.addBoundVariable(variableName).addStatement(ProvenStatement(assumption, outerContext.stepReference.withSuffix("a")))
    }
    override def replaceVariableName(newVariableName: String): Step = copy(variableName = newVariableName)
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(assumption).map(a => copy(assumption = a))
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      Naming(
        variableName,
        assumption.insertExternalParameters(numberOfParametersToInsert),
        statement.insertExternalParameters(numberOfParametersToInsert),
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert)),
        inference,
        premises,
        substitutions.insertExternalParameters(numberOfParametersToInsert))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newAssumption <- assumption.removeExternalParameters(numberOfParametersToRemove)
        newStatement <- statement.removeExternalParameters(numberOfParametersToRemove)
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
        newSubstitutions <- substitutions.removeExternalParameters(numberOfParametersToRemove)
      } yield Naming(variableName, newAssumption, newStatement, newSubsteps, inference, premises, newSubstitutions)
    }
    override def recalculateReferences(stepContext: StepContext, entryContext: EntryContext): Step = {
      val newSubsteps = substeps.recalculateReferences(specifyContext(stepContext), entryContext)
      val newPremises = premises.map(p => ProofHelper.findPremise(p.statement, stepContext, entryContext))
      copy(substeps = newSubsteps, premises = newPremises)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet + inference.id
    override def referencedDefinitions: Set[ExpressionDefinition] = assumption.referencedDefinitions ++ substeps.flatMap(_.referencedDefinitions).toSet
    override def referencedLines: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet ++ substeps.flatMap(_.referencedLines).toSet
    @JsonSerialize
    def referencedLinesForExtraction: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def length: Int = substeps.map(_.length).sum + 1
    override def serializedLines: Seq[String] = Seq(s"let $variableName ${assumption.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }

  object Naming {
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Naming] = {
      for {
        variableName <- Parser.singleWord
        assumption <- Statement.parser
        innerStepContext = stepContext
          .addBoundVariable(variableName)
          .addStatement(ProvenStatement(assumption, stepContext.stepReference.withSuffix("a")))
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        substeps <- listParser(entryContext, innerStepContext).inBraces
      } yield {
        val internalConclusion = substeps.flatMap(_.provenStatement).lastOption.getOrElse(throw new Exception("No conclusion for naming step"))
        val extractedConclusion = internalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
        val scopingStatement = entryContext.scopingStatementOption.getOrElse(throw new Exception("Naming step requires a scoping statement"))
        val deductionStatement = entryContext.deductionStatementOption.getOrElse(throw new Exception("Naming step requires a deduction statement"))
        val internalPremise = DefinedStatement(Seq(DefinedStatement(Seq(assumption, internalConclusion), deductionStatement)(Nil)), scopingStatement)(Seq(variableName))
        val substitutedPremises = inference.substitutePremisesAndValidateConclusion(extractedConclusion, substitutions, stepContext)
        val availablePremises = ProofHelper.getAvailablePremises(stepContext, entryContext)
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
    extends Step.WithSubsteps with WithVariable
  {
    val `type` = "scopedVariable"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => DefinedStatement(Seq(s), scopingStatement)(Seq(variableName)))
    }
    override def specifyContext(outerContext: StepContext): StepContext = {
      outerContext.addBoundVariable(variableName)
    }
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def replaceVariableName(newVariableName: String): Step = copy(variableName = newVariableName)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      ScopedVariable(
        variableName,
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert)),
        scopingStatement)
    }
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
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[ScopedVariable] = {
      val scopingStatement = entryContext.scopingStatementOption
        .getOrElse(throw new Exception("Scoped variable step could not find scoping statement"))
      for {
        variableName <- Parser.singleWord
        substeps <- listParser(entryContext, stepContext.addBoundVariable(variableName)).inBraces
      } yield ScopedVariable(variableName, substeps, scopingStatement)
    }
  }

  case class Target(statement: Statement) extends Step.WithoutSubsteps with Step.WithTopLevelStatement {
    val `type` = "target"
    override def provenStatement: Option[Statement] = Some(statement)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      Target(statement.insertExternalParameters(numberOfParametersToInsert))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        s <- statement.removeExternalParameters(numberOfParametersToRemove)
      } yield copy(statement = s)
    }
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    override def recalculateReferences(stepContext: StepContext, entryContext: EntryContext): Step = this
    override def referencedInferenceIds: Set[String] = Set.empty
    override def referencedDefinitions: Set[ExpressionDefinition] = statement.referencedDefinitions
    override def referencedLines: Set[PreviousLineReference] = Set.empty
    override def length = 1
    def serializedLines: Seq[String] = Seq(s"target ${statement.serialized}")
  }
  object Target {
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Target] = {
      for {
        statement <- Statement.parser
      } yield Target(statement)
    }
  }

  case class Elided(substeps: Seq[Step], highlightedInference: Option[Inference.Summary]) extends Step.WithSubsteps with WithoutVariable {
    val `type` = "elided"
    override def provenStatement: Option[Statement] = substeps.flatMap(_.provenStatement).lastOption
    override def specifyContext(outerContext: StepContext): StepContext = outerContext
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      Elided(
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert)),
        highlightedInference)
    }
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
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Elided] = {
      for {
        highlightedInference <- Inference.parser.tryOrNone
        substeps <- listParser.inBraces
      } yield Elided(substeps, highlightedInference)
    }
  }

  case class Assertion(
      statement: Statement,
      inference: Inference.Summary,
      premises: Seq[Premise],
      substitutions: Substitutions)
    extends Step.WithoutSubsteps with Step.WithTopLevelStatement
  {
    val `type`: String = "assertion"
    override def provenStatement: Option[Statement] = Some(statement)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      Assertion(
        statement.insertExternalParameters(numberOfParametersToInsert),
        inference,
        premises,
        substitutions.insertExternalParameters(numberOfParametersToInsert))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newStatement <- statement.removeExternalParameters(numberOfParametersToRemove)
        newPremises <- premises.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
        newSubstitutions <- substitutions.removeExternalParameters(numberOfParametersToRemove)
      } yield Assertion(newStatement, inference, newPremises, newSubstitutions)
    }
    override def recalculateReferences(stepContext: StepContext, entryContext: EntryContext): Step = {
      val newPremises = premises.map(p => ProofHelper.findPremise(p.statement, stepContext, entryContext))
      copy(premises = newPremises)
    }
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    override def referencedInferenceIds: Set[String] = Set(inference.id) ++ premises.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = statement.referencedDefinitions
    override def referencedLines: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def length: Int = 1
    override def serializedLines: Seq[String] = {
      Seq(s"prove ${statement.serialized} ${inference.id} ${inference.serializeSubstitutions(substitutions)}")
    }
    def pendingPremises: Map[Seq[Int], Premise.Pending] = {
      premises.flatMapWithIndex((p, i) => p.getPendingPremises(Seq(i)).toSeq).toMap
    }
    def isIncomplete: Boolean = premises.exists(_.isIncomplete)
  }
  object Assertion {
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Assertion] = {
      for {
        statement <- Statement.parser
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        premiseStatements = inference.substitutePremisesAndValidateConclusion(statement, substitutions, stepContext)
      } yield {
        val availablePremises = ProofHelper.getAvailablePremises(stepContext, entryContext)
        val premises = premiseStatements.map(s => availablePremises.find(_.statement == s).getOrElse(throw new Exception(s"Could not find premise $s")))
        Assertion(statement, inference, premises, substitutions)
      }
    }
  }

  case class SubProof(name: String, substeps: Seq[Step]) extends Step.WithSubsteps with WithoutVariable {
    val `type`: String = "subproof"
    override def specifyContext(outerContext: StepContext): StepContext = outerContext
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def provenStatement: Option[Statement] = substeps.flatMap(_.provenStatement).lastOption
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      SubProof(
        name,
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert)))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      } yield SubProof(name, newSubsteps)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = substeps.flatMap(_.referencedDefinitions).toSet
    override def referencedLines: Set[PreviousLineReference] = substeps.flatMap(_.referencedLines).toSet
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(s"subproof ($name) {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object SubProof {
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[SubProof] = {
      for {
        name <- Parser.allInParens
        substeps <- listParser.inBraces
      } yield SubProof(name, substeps)
    }
  }

  implicit class StepSeqOps(steps: Seq[Step]) {
    def recalculateReferences(outerContext: StepContext, entryContext: EntryContext): Seq[Step] = {
      steps.zipWithIndex.mapReduceWithPrevious[Step] { case (previousSteps, (oldStep, i)) =>
        oldStep.recalculateReferences(outerContext.addSteps(previousSteps).atIndex(i), entryContext)
      }
    }
  }

  def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Deduction.parser
      case "let" => Naming.parser
      case "take" => ScopedVariable.parser
      case "target" => Target.parser
      case "prove" => Assertion.parser
      case "elided" => Elided.parser
      case "subproof" => SubProof.parser
    }
  }
  def listParser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Seq[Step]] = {
    Parser.whileDefined[Step] { (previousSteps, index) =>
      val innerContext = stepContext.addSteps(previousSteps).atIndex(index)
      parser(entryContext, innerContext)
    }
  }
}
