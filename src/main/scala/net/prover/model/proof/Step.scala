package net.prover.model.proof

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.entries.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{DefinedStatement, Statement}
import scalaz.Functor
import scalaz.syntax.functor._

import scala.util.Try

@JsonIgnoreProperties(Array("substitutions", "deductionStatement", "scopingStatement", "isComplete"))
sealed trait Step {
  @JsonSerialize
  def provenStatement: Option[Statement]
  def getSubstep(index: Int, stepContext: StepContext): Option[(Step, StepContext)]
  def extractSubstep(index: Int): Option[Option[(Step, Step)]]
  def modifySubsteps[F[_] : Functor](outerContext: StepContext, f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Step]]
  def insertExternalParameters(numberOfParametersToRemove: Int): Step
  def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step]
  def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): Step
  def recalculateReferences(stepContext: StepContext): Step
  def isComplete: Boolean
  def referencedInferenceIds: Set[String]
  def referencedDefinitions: Set[ExpressionDefinition]
  @JsonSerialize
  def referencedLines: Set[PreviousLineReference]
  def length: Int
  def serializedLines: Seq[String]
}

object Step {
  sealed trait WithoutSubsteps extends Step {
    override def getSubstep(index: Int, outerContext: StepContext): Option[(Step, StepContext)] = None
    override def extractSubstep(index: Int): Option[Option[(Step, Step)]] = None
    override def modifySubsteps[F[_] : Functor](outerContext: StepContext, f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Step]] = None
  }
  sealed trait WithSubsteps extends Step {
    def substeps: Seq[Step]
    def specifyStepContext(outerContext: StepContext): StepContext = outerContext
    def replaceSubsteps(newSubsteps: Seq[Step]): Step
    def modifyStepForInsertion(step: Step): Step
    def modifyStepForExtraction(step: Step): Option[Step]
    override def isComplete: Boolean = substeps.forall(_.isComplete)
    override def getSubstep(index: Int, outerStepContext: StepContext): Option[(Step, StepContext)] = {
      substeps.splitAtIndexIfValid(index).map { case (before, step, _) =>
        val innerStepContext = specifyStepContext(outerStepContext).addSteps(before)
        (step, innerStepContext)
      }
    }
    override def extractSubstep(index: Int): Option[Option[(Step, Step)]] = {
      substeps.lift(index)
        .map(modifyStepForExtraction)
        .map(_.map(replaceSubsteps(substeps.removeAtIndex(index)) -> _))
    }
    override def recalculateReferences(stepContext: StepContext): Step = {
      val newSubsteps = substeps.recalculateReferences(specifyStepContext(stepContext))
      replaceSubsteps(newSubsteps)
    }
    override def modifySubsteps[F[_] : Functor](outerContext: StepContext, f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Step]] = {
      f(substeps, specifyStepContext(outerContext)).map(_.map(replaceSubsteps))
    }
  }
  sealed trait WithVariable extends Step.WithSubsteps {
    def variableName: String
    def replaceVariableName(newVariableName: String): Step
    override def specifyStepContext(outerContext: StepContext): StepContext = {
      super.specifyStepContext(outerContext).addBoundVariable(variableName)
    }
    override def modifyStepForInsertion(step: Step): Step = step.insertExternalParameters(1)
    override def modifyStepForExtraction(step: Step): Option[Step] = step.removeExternalParameters(1)
  }
  sealed trait WithoutVariable extends Step.WithSubsteps {
    override def specifyStepContext(outerContext: StepContext): StepContext = outerContext
    override def modifyStepForInsertion(step: Step): Step = step
    override def modifyStepForExtraction(step: Step): Option[Step] = Some(step)
  }
  sealed trait WithAssumption extends Step.WithSubsteps {
    def assumption: Statement
    override def specifyStepContext(outerContext: StepContext): StepContext = {
      super.specifyStepContext(outerContext).addStatement(assumption, "a")
    }
  }

  sealed trait WithTopLevelStatement extends Step {
    def updateStatement(f: Statement => Try[Statement]): Try[Step]
  }

  case class Deduction(
      assumption: Statement,
      substeps: Seq[Step],
      deductionStatement: StatementDefinition)
    extends Step.WithSubsteps with WithoutVariable with WithTopLevelStatement with WithAssumption
  {
    val `type` = "deduction"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => DefinedStatement(Seq(assumption, s), deductionStatement)(Nil))
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
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition,
      entryContext: EntryContext
    ): Deduction = {
      Deduction(
        assumption.replaceDefinition(oldDefinition, newDefinition),
        substeps.map(_.replaceDefinition(oldDefinition, newDefinition, entryContext)),
        entryContext.deductionDefinitionOption.get)
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
      val deductionDefinition = entryContext.deductionDefinitionOption
        .getOrElse(throw new Exception("Cannot prove a deduction without an appropriate statement definition"))
      for {
        assumption <- Statement.parser
        substeps <- listParser(entryContext, stepContext.addStatement(assumption, "a")).inBraces
      } yield Deduction(assumption, substeps, deductionDefinition)
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
    extends Step.WithSubsteps with WithTopLevelStatement with WithVariable with WithAssumption
  {
    val `type` = "naming"
    override def provenStatement: Option[Statement] = Some(statement)
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
        premises.map(_.insertExternalParameters(numberOfParametersToInsert)),
        substitutions.insertExternalParameters(numberOfParametersToInsert))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newAssumption <- assumption.removeExternalParameters(numberOfParametersToRemove)
        newStatement <- statement.removeExternalParameters(numberOfParametersToRemove)
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
        newPremises <- premises.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
        newSubstitutions <- substitutions.removeExternalParameters(numberOfParametersToRemove)
      } yield Naming(variableName, newAssumption, newStatement, newSubsteps, inference, newPremises, newSubstitutions)
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition,
      entryContext: EntryContext
    ): Naming = {
      Naming(
        variableName,
        assumption.replaceDefinition(oldDefinition, newDefinition),
        statement.replaceDefinition(oldDefinition, newDefinition),
        substeps.map(_.replaceDefinition(oldDefinition, newDefinition, entryContext)),
        inference.replaceDefinition(oldDefinition, newDefinition),
        premises.map(_.replaceDefinition(oldDefinition, newDefinition)),
        substitutions.replaceDefinition(oldDefinition, newDefinition))
    }
    override def recalculateReferences(stepContext: StepContext): Step = {
      val newSubsteps = substeps.recalculateReferences(specifyStepContext(stepContext))
      val newPremises = premises.map(p => stepContext.createPremise(p.statement))
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
      Seq(Seq("}", Premise.serialize(premises)).filter(_.nonEmpty).mkString(" "))
  }

  object Naming {
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Naming] = {
      for {
        variableName <- Parser.singleWord
        assumption <- Statement.parser
        innerStepContext = stepContext.addBoundVariable(variableName).addStatement(assumption, "a")
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        substeps <- listParser(entryContext, innerStepContext).inBraces
        internalConclusion = substeps.flatMap(_.provenStatement).lastOption.getOrElse(throw new Exception("No conclusion for naming step"))
        extractedConclusion = internalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
        premiseStatements = inference.substitutePremisesAndValidateConclusion(extractedConclusion, substitutions).getOrElse(throw new Exception("Could not apply substitutions"))
        premises <- premiseStatements.init.map(Premise.parser).traverse
        scopingDefinition = entryContext.scopingDefinitionOption.getOrElse(throw new Exception("Naming step requires a scoping statement"))
        deductionDefinition = entryContext.deductionDefinitionOption.getOrElse(throw new Exception("Naming step requires a deduction statement"))
        internalPremise = DefinedStatement(Seq(DefinedStatement(Seq(assumption, internalConclusion), deductionDefinition)(Nil)), scopingDefinition)(Seq(variableName))
        _ = if (internalPremise != premiseStatements.last) throw new Exception("Invalid naming premise")
      } yield {
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
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition,
      entryContext: EntryContext
    ): ScopedVariable = {
      ScopedVariable(
        variableName,
        substeps.map(_.replaceDefinition(oldDefinition, newDefinition, entryContext)),
        entryContext.scopingDefinitionOption.get)
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
      val scopingDefinition = entryContext.scopingDefinitionOption
        .getOrElse(throw new Exception("Scoped variable step could not find scoping statement"))
      for {
        variableName <- Parser.singleWord
        substeps <- listParser(entryContext, stepContext.addBoundVariable(variableName)).inBraces
      } yield ScopedVariable(variableName, substeps, scopingDefinition)
    }
  }

  case class Target(statement: Statement) extends Step.WithoutSubsteps with Step.WithTopLevelStatement {
    val `type` = "target"
    override def isComplete: Boolean = false
    override def provenStatement: Option[Statement] = Some(statement)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      Target(statement.insertExternalParameters(numberOfParametersToInsert))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        s <- statement.removeExternalParameters(numberOfParametersToRemove)
      } yield copy(statement = s)
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition,
      entryContext: EntryContext
    ): Target = Target(statement.replaceDefinition(oldDefinition, newDefinition))
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    override def recalculateReferences(stepContext: StepContext): Step = this
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

  case class Elided(substeps: Seq[Step], highlightedInference: Option[Inference.Summary], description: Option[String]) extends Step.WithSubsteps with WithoutVariable {
    val `type` = "elided"
    override def provenStatement: Option[Statement] = substeps.flatMap(_.provenStatement).lastOption
    override def replaceSubsteps(newSubsteps: Seq[Step]): Step = copy(substeps = newSubsteps)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      Elided(
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert)),
        highlightedInference,
        description)
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
      } yield Elided(newSubsteps, highlightedInference, description)
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition,
      entryContext: EntryContext
    ): Elided = {
      Elided(
        substeps.map(_.replaceDefinition(oldDefinition, newDefinition,entryContext)),
        highlightedInference.map(_.replaceDefinition(oldDefinition, newDefinition)),
        description)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = substeps.flatMap(_.referencedDefinitions).toSet
    override def referencedLines: Set[PreviousLineReference] = substeps.flatMap(_.referencedLines).toSet
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(("elided" +: (highlightedInference.map(_.id).toSeq ++ description.map(_.inParens).toSeq) :+ "{").mkString(" ")) ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Elided {
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Elided] = {
      for {
        highlightedInference <- Inference.parser.tryOrNone
        description <- Parser.allInParens.tryOrNone
        substeps <- listParser.inBraces
      } yield Elided(substeps, highlightedInference, description)
    }
    private def ifNecessary(substeps: Seq[Step], elider: Seq[Step] => Step): Option[Step] = {
      substeps match {
        case Nil => None
        case Seq(single) => Some(single)
        case _ => Some(elider(substeps))
      }
    }
    def ifNecessary(substeps: Seq[Step], inference: Inference): Option[Step] = {
      ifNecessary(substeps, forInference(inference))
    }
    def ifNecessary(substeps: Seq[Step], description: String): Option[Step] = {
      ifNecessary(substeps, Step.Elided(_, None, Some(description)))
    }

    def forInference(inference: Inference): Seq[Step] => Step.Elided = {
      Step.Elided(_, Some(inference.summary), None)
    }
    def forDescription(description: String): Seq[Step] => Step.Elided = {
      Step.Elided(_, None, Some(description))
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
    override def isComplete: Boolean = true
    override def provenStatement: Option[Statement] = Some(statement)
    override def insertExternalParameters(numberOfParametersToInsert: Int): Step = {
      Assertion(
        statement.insertExternalParameters(numberOfParametersToInsert),
        inference,
        premises.map(_.insertExternalParameters(numberOfParametersToInsert)),
        substitutions.insertExternalParameters(numberOfParametersToInsert))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int): Option[Step] = {
      for {
        newStatement <- statement.removeExternalParameters(numberOfParametersToRemove)
        newPremises <- premises.map(_.removeExternalParameters(numberOfParametersToRemove)).traverseOption
        newSubstitutions <- substitutions.removeExternalParameters(numberOfParametersToRemove)
      } yield Assertion(newStatement, inference, newPremises, newSubstitutions)
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition,
      entryContext: EntryContext
    ): Assertion = {
      Assertion(
        statement.replaceDefinition(oldDefinition, newDefinition),
        inference.replaceDefinition(oldDefinition, newDefinition),
        premises.map(_.replaceDefinition(oldDefinition, newDefinition)),
        substitutions.replaceDefinition(oldDefinition, newDefinition))
    }
    override def recalculateReferences(stepContext: StepContext): Step = {
      val newPremises = premises.map(p => stepContext.createPremise(p.statement))
      copy(premises = newPremises)
    }
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    override def referencedInferenceIds: Set[String] = Set(inference.id) ++ premises.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = statement.referencedDefinitions
    override def referencedLines: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def length: Int = 1
    override def serializedLines: Seq[String] = {
      Seq(Seq("prove", statement.serialized, inference.id, inference.serializeSubstitutions(substitutions), Premise.serialize(premises)).filter(_.nonEmpty).mkString(" "))
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
        premiseStatements = inference.substitutePremisesAndValidateConclusion(statement, substitutions).getOrElse(throw new Exception("Could not apply substitutions"))
        premises <- premiseStatements.map(Premise.parser).traverse
        _ = inference.validatePremisesAndConclusion(premises.map(_.statement), statement, substitutions)
      } yield {
        Assertion(statement, inference, premises, substitutions)
      }
    }
  }

  case class SubProof(name: String, substeps: Seq[Step]) extends Step.WithSubsteps with WithoutVariable {
    val `type`: String = "subproof"
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
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition,
      entryContext: EntryContext
    ): SubProof = {
      SubProof(
        name,
        substeps.map(_.replaceDefinition(oldDefinition, newDefinition, entryContext)))
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
    def recalculateReferences(outerStepContext: StepContext): Seq[Step] = {
      steps.zipWithIndex.mapFold(outerStepContext) { case (currentStepContext, (step, index)) =>
        val innerStepContext = currentStepContext.atIndex(index)
        val newStep = step.recalculateReferences(innerStepContext)
        currentStepContext.addStep(newStep, innerStepContext.stepReference) -> newStep
      }._2
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
    Parser.foldWhileDefined[Step, StepContext](stepContext) { (_, index, currentStepContext) =>
      val innerStepContext = currentStepContext.atIndex(index)
      parser(entryContext, innerStepContext)
        .mapMap(step => step -> currentStepContext.addStep(step, innerStepContext.stepReference))
    }.map(_._1)
  }
}
