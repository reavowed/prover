package net.prover.model.proof

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonIgnoreProperties}
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.controllers.models.StepWithReferenceChange
import net.prover.exceptions.InferenceReplacementException
import net.prover.model._
import net.prover.model.definitions.{DeductionDefinition, Definitions, ExpressionDefinition, GeneralizationDefinition, StatementDefinition}
import net.prover.model.expressions.{DefinedStatement, Statement}
import scalaz.Functor
import scalaz.syntax.functor._

import scala.util.{Failure, Success, Try}

@JsonIgnoreProperties(Array("substitutions", "isComplete"))
sealed trait Step {
  def provenStatement: Option[Statement]
  def getSubstep(index: Int, stepContext: StepContext): Option[(Step, StepContext)]
  def modifySubsteps[F[_] : Functor](outerContext: StepContext)(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Step]]
  def insertExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Step
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Step]
  def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): Step
  def replaceInference(
    oldInference: Inference,
    newInference: Inference,
    stepProvingContext: StepProvingContext
  ): Try[Step]
  def clearInference(inferenceToClear: Inference): Step
  def recalculateReferences(stepContext: StepContext, provingContext: ProvingContext): (Step, Seq[StepWithReferenceChange])
  def isComplete(definitions: Definitions): Boolean
  def referencedInferenceIds: Set[String]
  def referencedDefinitions: Set[ExpressionDefinition]
  def referencedLines: Set[PreviousLineReference] = recursivePremises.flatMap(_.referencedLines).toSet
  def recursivePremises: Seq[Premise]
  def length: Int
  def serializedLines: Seq[String]
  override def toString: String = serializedLines.mkString("\n")
}

object Step {
  sealed trait WithoutSubsteps extends Step {
    override def getSubstep(index: Int, outerContext: StepContext): Option[(Step, StepContext)] = None
    override def modifySubsteps[F[_] : Functor](outerContext: StepContext)(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Step]] = None
  }
  sealed trait WithSubsteps extends Step {
    def substeps: Seq[Step]
    def specifyStepContext(outerContext: StepContext): StepContext = outerContext
    def contextForChild(outerContext: StepContext, index: Int): StepContext = specifyStepContext(outerContext).addSteps(substeps.take(index)).atIndex(index)
    def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step
    override def isComplete(definitions: Definitions): Boolean = substeps.forall(_.isComplete(definitions))
    override def getSubstep(index: Int, outerStepContext: StepContext): Option[(Step, StepContext)] = {
      substeps.splitAtIndexIfValid(index).map { case (before, step, _) =>
        val innerStepContext = specifyStepContext(outerStepContext).addSteps(before).atIndex(index)
        (step, innerStepContext)
      }
    }
    override def replaceInference(
      oldInference: Inference,
      newInference: Inference,
      stepProvingContext: StepProvingContext
    ): Try[Step] = {
      substeps.replaceInference(oldInference, newInference, stepProvingContext.updateStepContext(specifyStepContext)).map(replaceSubsteps(_, stepProvingContext.stepContext))
    }
    override def recalculateReferences(stepContext: StepContext, provingContext: ProvingContext): (Step, Seq[StepWithReferenceChange]) = {
      substeps.recalculateReferences(specifyStepContext(stepContext), provingContext)
        .mapLeft(replaceSubsteps(_, stepContext))
    }
    def modifySubstepsDirectly[F[_] : Functor](outerContext: StepContext)(f: (Seq[Step], StepContext) => F[Seq[Step]]): F[Step] = {
      f(substeps, specifyStepContext(outerContext)).map(replaceSubsteps(_, outerContext))
    }
    override def modifySubsteps[F[_] : Functor](outerContext: StepContext)(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Step]] = {
      f(substeps, specifyStepContext(outerContext)).map(_.map(replaceSubsteps(_, outerContext)))
    }
  }
  sealed trait WithVariable extends Step.WithSubsteps {
    def variableName: String
    def replaceVariableName(newVariableName: String): Step
    override def specifyStepContext(outerContext: StepContext): StepContext = {
      super.specifyStepContext(outerContext).addBoundVariable(variableName)
    }
  }
  sealed trait WithoutVariable extends Step.WithSubsteps {
    override def specifyStepContext(outerContext: StepContext): StepContext = outerContext
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
      deductionDefinition: DeductionDefinition)
    extends Step.WithSubsteps with WithoutVariable with WithTopLevelStatement with WithAssumption
  {
    val `type` = "deduction"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => deductionDefinition(assumption, s))
    }
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Deduction = copy(substeps = newSubsteps)
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(assumption).map(a => copy(assumption = a))
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Step = {
      Deduction(
        assumption.insertExternalParameters(numberOfParametersToInsert, internalDepth),
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)),
        deductionDefinition)
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Step] = {
      for {
        newAssumption <- assumption.removeExternalParameters(numberOfParametersToRemove, internalDepth)
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption
      } yield Deduction(newAssumption, newSubsteps, deductionDefinition)
    }
    override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): Deduction = {
      Deduction(
        assumption.replaceDefinitions(expressionDefinitionReplacements),
        substeps.map(_.replaceDefinitions(expressionDefinitionReplacements, entryContext)),
        entryContext.deductionDefinitionOption.get)
    }
    override def clearInference(inferenceToClear: Inference): Step = {
      Deduction(
        assumption,
        substeps.clearInference(inferenceToClear),
        deductionDefinition)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
    override def referencedDefinitions: Set[ExpressionDefinition] = assumption.referencedDefinitions ++ substeps.flatMap(_.referencedDefinitions).toSet + deductionDefinition.statementDefinition
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
      substitutions: Substitutions,
      @JsonIgnore generalizationDefinition: GeneralizationDefinition,
      @JsonIgnore deductionDefinition: DeductionDefinition)
    extends Step.WithSubsteps with WithTopLevelStatement with WithVariable with WithAssumption
  {
    val `type` = "naming"
    override def isComplete(definitions: Definitions): Boolean = super.isComplete(definitions) && premises.forall(_.isComplete) && definitions.isInferenceComplete(inference)
    override def provenStatement: Option[Statement] = Some(statement)
    override def replaceVariableName(newVariableName: String): Step = copy(variableName = newVariableName)
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = {
      val newInternalConclusion = newSubsteps.flatMap(_.provenStatement).lastOption.getOrElse(throw new Exception("No conclusion for naming step"))
      val newConclusion = newInternalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
      val newInternalPremise = generalizationDefinition(variableName, deductionDefinition(assumption, newInternalConclusion))
      val newSubstitutions = inference.premises.zip(premises.map(_.statement) :+ newInternalPremise)
        .foldLeft(Substitutions.Possible.empty) { case (substitutions, (inferencePremise, premise)) => inferencePremise.calculateSubstitutions(premise, substitutions)(stepContext).getOrElse(throw new Exception("Could not calculate substitutions from naming premise"))}
        .confirmTotality(inference.variableDefinitions)
        .getOrElse(throw new Exception("Naming substitutions were not total"))
      Naming(
        variableName,
        assumption,
        newConclusion,
        newSubsteps,
        inference,
        premises,
        newSubstitutions,
        generalizationDefinition,
        deductionDefinition)
    }
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(assumption).map(a => copy(assumption = a))
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Step = {
      Naming(
        variableName,
        assumption.insertExternalParameters(numberOfParametersToInsert, internalDepth + 1),
        statement.insertExternalParameters(numberOfParametersToInsert, internalDepth),
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth + 1)),
        inference,
        premises.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)),
        substitutions.insertExternalParameters(numberOfParametersToInsert, internalDepth),
        generalizationDefinition,
        deductionDefinition)
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Step] = {
      for {
        newAssumption <- assumption.removeExternalParameters(numberOfParametersToRemove, internalDepth + 1)
        newStatement <- statement.removeExternalParameters(numberOfParametersToRemove, internalDepth)
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth + 1)).traverseOption
        newPremises <- premises.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption
        newSubstitutions <- substitutions.removeExternalParameters(numberOfParametersToRemove, internalDepth)
      } yield Naming(variableName, newAssumption, newStatement, newSubsteps, inference, newPremises, newSubstitutions, generalizationDefinition, deductionDefinition)
    }
    override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): Naming = {
      Naming(
        variableName,
        assumption.replaceDefinitions(expressionDefinitionReplacements),
        statement.replaceDefinitions(expressionDefinitionReplacements),
        substeps.map(_.replaceDefinitions(expressionDefinitionReplacements, entryContext)),
        inference.replaceDefinitions(expressionDefinitionReplacements),
        premises.map(_.replaceDefinitions(expressionDefinitionReplacements)),
        substitutions.replaceDefinitions(expressionDefinitionReplacements),
        entryContext.generalizationDefinitionOption.get,
        entryContext.deductionDefinitionOption.get)
    }
    override def replaceInference(oldInference: Inference, newInference: Inference, stepProvingContext: StepProvingContext): Try[Step] = {
      if (inference == oldInference) {
        for {
          updatedStep <- Assertion(statement, inference, premises, substitutions).replaceInference(oldInference, newInference, stepProvingContext)
          updatedAssertion <- updatedStep.asOptionalInstanceOf[Step.Assertion].failIfUndefined(InferenceReplacementException.AtStep("Cannot replace naming with an elided step")(stepProvingContext.stepContext))
          updatedSubsteps <- substeps.replaceInference(oldInference, newInference, stepProvingContext.updateStepContext(specifyStepContext))
        } yield Naming(
          variableName,
          assumption,
          statement,
          updatedSubsteps,
          updatedAssertion.inference,
          updatedAssertion.premises,
          updatedAssertion.substitutions,
          generalizationDefinition,
          deductionDefinition)
      } else {
        super.replaceInference(oldInference, newInference, stepProvingContext)
      }
    }
    override def clearInference(inferenceToClear: Inference): Step = {
      if (inferenceToClear == inference) {
        Step.Target(statement)
      } else {
        copy(substeps = substeps.clearInference(inferenceToClear))
      }
    }
    override def recalculateReferences(stepContext: StepContext, provingContext: ProvingContext): (Step, Seq[StepWithReferenceChange]) = {
      val (newSubsteps, innerStepsWithReferenceChanges) = substeps.recalculateReferences(specifyStepContext(stepContext), provingContext)
      val newPremises = premises.map(p => StepProvingContext(stepContext, provingContext).createPremise(p.statement))
      val newStep = copy(substeps = newSubsteps, premises = newPremises)
      val stepsWithReferenceChanges = if (premises == newPremises) innerStepsWithReferenceChanges else innerStepsWithReferenceChanges :+ StepWithReferenceChange(newStep, stepContext.stepReference.stepPath)
      (newStep, stepsWithReferenceChanges)
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet + inference.id
    override def referencedDefinitions: Set[ExpressionDefinition] = assumption.referencedDefinitions ++ substeps.flatMap(_.referencedDefinitions).toSet
    override def recursivePremises: Seq[Premise] = premises ++ substeps.flatMap(_.recursivePremises)
    @JsonSerialize
    def referencedLinesForExtraction: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def length: Int = substeps.map(_.length).sum + 1
    override def serializedLines: Seq[String] = Seq(s"let $variableName ${assumption.serialized} ${inference.id} ${substitutions.serialize} {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq(Seq("}", Premise.serialize(premises)).filter(_.nonEmpty).mkString(" "))
  }
  object Naming {
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Naming] = {
      for {
        variableName <- Parser.singleWord
        assumption <- Statement.parser(ExpressionParsingContext.atStep(implicitly, stepContext.addBoundVariable(variableName)))
        innerStepContext = stepContext.addBoundVariable(variableName).addStatement(assumption, "a")
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        substeps <- listParser(entryContext, innerStepContext).inBraces
        internalConclusion = substeps.flatMap(_.provenStatement).lastOption.getOrElse(throw new Exception("No conclusion for naming step"))
        extractedConclusion = internalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
        premiseStatements = inference.substitutePremisesAndValidateConclusion(extractedConclusion, substitutions).getOrElse(throw new Exception("Could not apply substitutions"))
        premises <- premiseStatements.init.map(Premise.parser).traverse
        generalizationDefinition = entryContext.generalizationDefinitionOption.getOrElse(throw new Exception("Naming step requires a generalization statement"))
        deductionDefinition = entryContext.deductionDefinitionOption.getOrElse(throw new Exception("Naming step requires a deduction statement"))
        internalPremise = generalizationDefinition(variableName, deductionDefinition(assumption, internalConclusion))
        _ = if (internalPremise != premiseStatements.last) throw new Exception("Invalid naming premise")
      } yield {
        Naming(variableName, assumption, extractedConclusion, substeps, inference, premises, substitutions, generalizationDefinition, deductionDefinition)
      }
    }
  }

  case class Generalization(
      variableName: String,
      substeps: Seq[Step],
      generalizationDefinition: GeneralizationDefinition)
    extends Step.WithSubsteps with WithVariable
  {
    val `type` = "generalization"
    override def provenStatement: Option[Statement] = {
      substeps.lastOption.flatMap(_.provenStatement).map(s => generalizationDefinition(variableName, s))
    }
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = copy(substeps = newSubsteps)
    override def replaceVariableName(newVariableName: String): Step = copy(variableName = newVariableName)
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Step = {
      Generalization(
        variableName,
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth + 1)),
        generalizationDefinition)
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Step] = {
      for {
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth + 1)).traverseOption
      } yield Generalization(variableName, newSubsteps, generalizationDefinition)
    }
    override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): Generalization = {
      Generalization(
        variableName,
        substeps.map(_.replaceDefinitions(expressionDefinitionReplacements, entryContext)),
        entryContext.generalizationDefinitionOption.get)
    }
    override def clearInference(inferenceToClear: Inference): Step = {
      copy(substeps = substeps.clearInference(inferenceToClear))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = substeps.flatMap(_.referencedDefinitions).toSet + generalizationDefinition.statementDefinition
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = Seq(s"take $variableName {") ++
      substeps.flatMap(_.serializedLines).indent ++
      Seq("}")
  }
  object Generalization {
    def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Generalization] = {
      val generalizationDefinition = entryContext.generalizationDefinitionOption
        .getOrElse(throw new Exception("Generalization step could not find generalization statement"))
      for {
        variableName <- Parser.singleWord
        substeps <- listParser(entryContext, stepContext.addBoundVariable(variableName)).inBraces
      } yield Generalization(variableName, substeps, generalizationDefinition)
    }
  }

  case class Target(statement: Statement) extends Step.WithoutSubsteps with Step.WithTopLevelStatement {
    val `type` = "target"
    override def isComplete(definitions: Definitions): Boolean = false
    override def provenStatement: Option[Statement] = Some(statement)
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Step = {
      Target(statement.insertExternalParameters(numberOfParametersToInsert, internalDepth))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Step] = {
      for {
        s <- statement.removeExternalParameters(numberOfParametersToRemove, internalDepth)
      } yield copy(statement = s)
    }
    override def replaceInference(oldInference: Inference, newInference: Inference, stepProvingContext: StepProvingContext): Try[Step] = Success(this)
    override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): Target = Target(statement.replaceDefinitions(expressionDefinitionReplacements))
    override def clearInference(inferenceToClear: Inference): Step = {
      this
    }
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    override def recalculateReferences(stepContext: StepContext, provingContext: ProvingContext): (Step, Seq[StepWithReferenceChange]) = (this, Nil)
    override def referencedInferenceIds: Set[String] = Set.empty
    override def referencedDefinitions: Set[ExpressionDefinition] = statement.referencedDefinitions
    override def recursivePremises: Seq[Premise] = Nil
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
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = copy(substeps = newSubsteps)
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Step = {
      Elided(
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)),
        highlightedInference,
        description)
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Step] = {
      for {
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption
      } yield Elided(newSubsteps, highlightedInference, description)
    }

    override def replaceInference(oldInference: Inference, newInference: Inference, stepProvingContext: StepProvingContext): Try[Step] = {
      if (highlightedInference.contains(oldInference)) {
        for {
          updatedSubsteps <- substeps.replaceInference(oldInference, newInference, stepProvingContext.updateStepContext(specifyStepContext))
        } yield copy(highlightedInference = Some(newInference.summary), substeps = updatedSubsteps)
      } else {
        super.replaceInference(oldInference, newInference, stepProvingContext)
      }
    }
    override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): Elided = {
      Elided(
        substeps.map(_.replaceDefinitions(expressionDefinitionReplacements,entryContext)),
        highlightedInference.map(_.replaceDefinitions(expressionDefinitionReplacements)),
        description)
    }
    override def clearInference(inferenceToClear: Inference): Step = {
      if (highlightedInference.contains(inferenceToClear)) {
        Target(provenStatement.get)
      } else {
        copy(substeps = substeps.clearInference(inferenceToClear))
      }
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet ++ highlightedInference.map(_.id).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = substeps.flatMap(_.referencedDefinitions).toSet
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
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
    def ifNecessary(substeps: Seq[Step], elider: Seq[Step] => Step): Option[Step] = {
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
    def ifNecessary(substeps: Seq[Step], inferenceOption: Option[Inference], description: String): Option[Step] = {
      ifNecessary(substeps, inferenceOption.map(forInference).getOrElse(forDescription(description)))
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
    override def isComplete(definitions: Definitions): Boolean = premises.forall(_.isComplete) && definitions.isInferenceComplete(inference)
    override def provenStatement: Option[Statement] = Some(statement)
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Step = {
      Assertion(
        statement.insertExternalParameters(numberOfParametersToInsert, internalDepth),
        inference,
        premises.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)),
        substitutions.insertExternalParameters(numberOfParametersToInsert, internalDepth))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Step] = {
      for {
        newStatement <- statement.removeExternalParameters(numberOfParametersToRemove, internalDepth)
        newPremises <- premises.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption
        newSubstitutions <- substitutions.removeExternalParameters(numberOfParametersToRemove, internalDepth)
      } yield Assertion(newStatement, inference, newPremises, newSubstitutions)
    }
    override def replaceInference(oldInference: Inference, newInference: Inference, stepProvingContext: StepProvingContext): Try[Step] = {
      implicit val spc = stepProvingContext
      if (inference == oldInference) {
        val substitutionsOption = (for {
          inferenceExtraction <- stepProvingContext.provingContext.inferenceExtractionsByInferenceId(newInference.id)
          substitutionsAfterConclusion <- inferenceExtraction.conclusion.calculateSubstitutions(statement).toSeq
          substitutionsAfterPremises <- inferenceExtraction.premises.zipStrict(premises).flatMap(_.foldLeft(Option(substitutionsAfterConclusion)) { case (so, (ep, p)) => so.flatMap(s => ep.calculateSubstitutions(p.statement, s)) }).toSeq
          substitutions <- substitutionsAfterPremises.confirmTotality(inferenceExtraction.variableDefinitions).toSeq
        } yield (inferenceExtraction, substitutions)).headOption
        for {
          (inferenceExtraction, substitutions) <- substitutionsOption.failIfUndefined(InferenceReplacementException.AtStep("Could not find extraction option")(stepProvingContext.stepContext))
          extractionStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions).failIfUndefined(InferenceReplacementException.AtStep("Could not apply extraction")(stepProvingContext.stepContext))
        } yield extractionStep.step
      } else Success(this)
    }
    override def clearInference(inferenceToClear: Inference): Step = {
      if (inferenceToClear == inference) {
        Target(statement)
      } else {
        this
      }
    }
    override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): Assertion = {
      Assertion(
        statement.replaceDefinitions(expressionDefinitionReplacements),
        inference.replaceDefinitions(expressionDefinitionReplacements),
        premises.map(_.replaceDefinitions(expressionDefinitionReplacements)),
        substitutions.replaceDefinitions(expressionDefinitionReplacements))
    }
    override def recalculateReferences(stepContext: StepContext, provingContext: ProvingContext): (Step, Seq[StepWithReferenceChange]) = {
      val newPremises = premises.map(p => StepProvingContext(stepContext, provingContext).createPremise(p.statement))
      val newStep = copy(premises = newPremises)
      (newStep, if (newPremises == premises) Nil else Seq(StepWithReferenceChange(newStep, stepContext.stepReference.stepPath)))
    }
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    @JsonSerialize
    def referencedLinesForAssertion: Set[PreviousLineReference] = premises.flatMap(_.referencedLines).toSet
    override def referencedInferenceIds: Set[String] = Set(inference.id) ++ premises.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = statement.referencedDefinitions
    override def recursivePremises: Seq[Premise] = premises
    override def length: Int = 1
    override def serializedLines: Seq[String] = {
      Seq(Seq("prove", statement.serialized, inference.id, substitutions.serialize, Premise.serialize(premises)).filter(_.nonEmpty).mkString(" "))
    }
    def pendingPremises: Map[Seq[Int], Premise.Pending] = {
      premises.flatMapWithIndex((p, i) => p.getPendingPremises(Seq(i)).toSeq).toMap
    }
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

    def forInference(inference: Inference, substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[Assertion] = {
      for {
        premises <- inference.substitutePremises(substitutions)
        conclusion <- inference.substituteConclusion(substitutions)
      } yield Assertion(conclusion, inference.summary, premises.map(Premise.Pending), substitutions.restrictTo(inference.variableDefinitions))
    }
  }

  case class SubProof(name: String, substeps: Seq[Step]) extends Step.WithSubsteps with WithoutVariable {
    val `type`: String = "subproof"
    override def replaceSubsteps(newSubsteps: Seq[Step], stepContext: StepContext): Step = copy(substeps = newSubsteps)
    override def provenStatement: Option[Statement] = substeps.flatMap(_.provenStatement).lastOption
    override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int): Step = {
      SubProof(
        name,
        substeps.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)))
    }
    override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Step] = {
      for {
        newSubsteps <- substeps.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption
      } yield SubProof(name, newSubsteps)
    }
    override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): SubProof = {
      SubProof(
        name,
        substeps.map(_.replaceDefinitions(expressionDefinitionReplacements, entryContext)))
    }
    override def clearInference(inferenceToClear: Inference): Step = {
      copy(substeps = substeps.clearInference(inferenceToClear))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referencedDefinitions: Set[ExpressionDefinition] = substeps.flatMap(_.referencedDefinitions).toSet
    override def recursivePremises: Seq[Premise] = substeps.flatMap(_.recursivePremises)
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
    def recalculateReferences(outerStepContext: StepContext, provingContext: ProvingContext): (Seq[Step], Seq[StepWithReferenceChange]) = {
      steps.zipWithIndex.mapFold(outerStepContext) { case (currentStepContext, (step, index)) =>
        val innerStepContext = currentStepContext.atIndex(index)
        val (newStep, stepsWithReferenceChanges) = step.recalculateReferences(innerStepContext, provingContext)
        currentStepContext.addStep(newStep, innerStepContext.stepReference) -> (newStep, stepsWithReferenceChanges)
      }._2.split.mapRight(_.flatten)
    }
    def replaceInference(
      oldInference: Inference,
      newInference: Inference,
      outerStepProvingContext: StepProvingContext
    ): Try[Seq[Step]] = {
      steps.zipWithIndex.mapFoldTry(outerStepProvingContext) { case (currentStepProvingContext, (step, index)) =>
        val innerStepProvingContext = currentStepProvingContext.updateStepContext(_.atIndex(index))
        for {
          newStep <- step.replaceInference(oldInference, newInference, innerStepProvingContext)
        } yield currentStepProvingContext.updateStepContext(_.addStep(newStep, innerStepProvingContext.stepContext.stepReference)) -> newStep
      }.map(_._2)
    }
    def clearInference(inference: Inference): Seq[Step] = steps.map(_.clearInference(inference))
  }

  def parser(implicit entryContext: EntryContext, stepContext: StepContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "assume" => Deduction.parser
      case "let" => Naming.parser
      case "take" => Generalization.parser
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
