package net.prover.model.proof

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonIgnoreProperties}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.unwrapping.{DeductionUnwrapper, GeneralizationUnwrapper, Unwrapper}
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction}

import scala.annotation.tailrec
import scala.util.Try

@JsonIgnoreProperties(Array("substitutions", "isComplete", "unwrappers", "assertionStep", "premiseSteps", "innerSteps"))
sealed trait Step extends StepLike {
  @JsonSerialize
  def `type`: String = getClass.getSimpleName.stripSuffix("Step").decapitalize
  override def toString: String = serializedLines.mkString("\n")
}

object Step {
  sealed trait WithoutSubsteps extends Step
  sealed trait WithSubsteps extends Step {
    @JsonSerialize
    def substeps: Seq[Step]
    def specifyStepContext(outerContext: StepContext): StepContext = outerContext.forChild()
    def replaceSubsteps(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = {
      if (newSubsteps.lastOption.map(_.statement).contains(substeps.last.statement)) {
        replaceSubstepsInternal(newSubsteps)
      } else {
        replaceSubstepsInternal(newSubsteps :+ TargetStep(substeps.last.statement))
      }
    }
    protected def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step
  }
  sealed trait WithVariable extends Step.WithSubsteps {
    def variableName: String
    def replaceVariableName(newVariableName: String): Step
    override def specifyStepContext(outerContext: StepContext): StepContext = {
      super.specifyStepContext(outerContext.addBoundVariable(variableName))
    }
  }
  sealed trait WithAssumption extends Step.WithSubsteps {
    def assumption: Statement
    override def specifyStepContext(outerContext: StepContext): StepContext = {
      super.specifyStepContext(outerContext.addAssumption(assumption))
    }
  }
  sealed trait WithTopLevelStatement extends Step {
    def updateStatement(f: Statement => Try[Statement]): Try[Step]
  }

  sealed trait PremiseDerivation extends Step
  sealed trait InferenceApplication extends Step {
    @JsonSerialize
    def inference: Inference.Summary
  }
  sealed trait InferenceApplicationWithoutPremises extends InferenceApplication with PremiseDerivation
  sealed trait AssertionOrExtraction extends InferenceApplicationWithoutPremises

  sealed trait Autogenerated extends Step.WithSubsteps {
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = {
      substeps.flatMap(_.serializedLines).indentInLabelledBracesIfPresent(`type`)
    }
  }
  sealed trait Wrapped extends Step.Autogenerated {
    def unwrappers: Seq[Unwrapper]
    def innerSteps: Seq[Step]
    override def statement: Statement = unwrappers.addToStatement(innerSteps.last.statement)
    override def substeps: Seq[Step] = unwrappers.rewrap(innerSteps)
  }
  object Wrapped {
    def extractWrappers[T](steps: Seq[Step])(f: (Seq[Unwrapper], Seq[Step]) => T)(implicit provingContext: ProvingContext): T = {
      @tailrec def helper(unwrappers: Seq[Unwrapper], steps: Seq[Step]): T = {
        steps match {
          case Seq(Step.GeneralizationStep(variableName, substeps, generalizationDefinition)) =>
            helper(unwrappers :+ GeneralizationUnwrapper(variableName, generalizationDefinition, provingContext.specificationInferenceOption.get._1), substeps)
          case Seq(Step.DeductionStep(assumption, substeps, deductionDefinition)) =>
            helper(unwrappers :+ DeductionUnwrapper(assumption, deductionDefinition, provingContext.deductionEliminationInferenceOption.get._1), substeps)
          case steps =>
            f(unwrappers, steps)
        }
      }
      helper(Nil, steps)
    }
  }

  case class TargetStep(statement: Statement) extends Step.WithoutSubsteps with Step.WithTopLevelStatement {
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    override def length = 1
    def serializedLines: Seq[String] = Seq(s"target ${statement.serialized}")
  }
  object TargetStep {
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[TargetStep] = {
      for {
        statement <- Statement.parser
      } yield TargetStep(statement)
    }
  }

  case class AssertionStep(
    statement: Statement,
    inference: Inference.Summary,
    premises: Seq[Premise],
    substitutions: Substitutions)
    extends Step.AssertionOrExtraction
      with Step.WithoutSubsteps
      with Step.WithTopLevelStatement
  {
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(statement).map(a => copy(statement = a))
    override def length: Int = 1
    override def serializedLines: Seq[String] = {
      Seq(Seq(AssertionStep.label, statement.serialized, inference.id, substitutions.serialize, Premise.serialize(premises)).filter(_.nonEmpty).mkString(" "))
    }
    def pendingPremises: Map[Seq[Int], Premise.Pending] = {
      premises.flatMapWithIndex((p, i) => p.getPendingPremises(Seq(i)).toSeq).toMap
    }
  }
  object AssertionStep {
    val label: String = "prove"
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[AssertionStep] = {
      for {
        statement <- Statement.parser
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        premiseStatements = inference.substitutePremises(substitutions).getOrElse(throw new Exception("Could not apply substitutions"))
        premises <- premiseStatements.map(Premise.parser).traverse
        _ = inference.validatePremisesAndConclusion(premises.map(_.statement), statement, substitutions).getOrElse(throw new Exception("Could not validate premises and conclusion"))
      } yield {
        AssertionStep(statement, inference, premises, substitutions)
      }
    }
    def forInference(inference: Inference, substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[AssertionStep] = {
      for {
        premises <- inference.substitutePremises(substitutions)
        conclusion <- inference.substituteConclusion(substitutions)
      } yield AssertionStep(conclusion, inference.summary, premises.map(Premise.Pending), substitutions.restrictTo(inference.variableDefinitions))
    }
  }

  case class DeductionStep(
      assumption: Statement,
      substeps: Seq[Step],
      deductionDefinition: DeductionDefinition)
    extends Step.WithSubsteps with WithTopLevelStatement with WithAssumption
  {
    override def statement: Statement = {
      deductionDefinition(assumption, substeps.last.statement)
    }
    override def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): DeductionStep = {
      copy(substeps = newSubsteps)
    }
    override def updateStatement(f: Statement => Try[Statement]): Try[Step] = f(assumption).map(a => copy(assumption = a))
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = {
      substeps.flatMap(_.serializedLines).indentInLabelledBracesIfPresent(s"assume ${assumption.serialized}")
    }
  }
  object DeductionStep {
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[DeductionStep] = {
      val deductionDefinition = provingContext.deductionDefinitionOption
        .getOrElse(throw new Exception("Cannot prove a deduction without an appropriate statement definition"))
      for {
        assumption <- Statement.parser
        substeps <- childStepsParser(stepContext.addAssumption(assumption), provingContext).inBraces
      } yield DeductionStep(assumption, substeps, deductionDefinition)
    }
  }

  case class GeneralizationStep(
      variableName: String,
      substeps: Seq[Step],
      generalizationDefinition: GeneralizationDefinition)
    extends Step.WithSubsteps with WithVariable
  {
    override def statement: Statement = {
      generalizationDefinition(variableName, substeps.last.statement)
    }
    override def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = copy(substeps = newSubsteps)
    override def replaceVariableName(newVariableName: String): Step = copy(variableName = newVariableName)
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = {
      substeps.flatMap(_.serializedLines).indentInLabelledBracesIfPresent(s"take $variableName")
    }
  }
  object GeneralizationStep {
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[GeneralizationStep] = {
      val generalizationDefinition = provingContext.generalizationDefinitionOption
        .getOrElse(throw new Exception("Generalization step could not find generalization statement"))
      for {
        variableName <- Parser.singleWord
        substeps <- childStepsParser(stepContext.addBoundVariable(variableName), provingContext).inBraces
      } yield GeneralizationStep(variableName, substeps, generalizationDefinition)
    }
  }

  case class NamingStep(
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
    override def replaceVariableName(newVariableName: String): Step = copy(variableName = newVariableName)
    override def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = {
      val newInternalConclusion = newSubsteps.last.statement
      val newConclusion = newInternalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
      val newInternalPremise = generalizationDefinition(variableName, deductionDefinition(assumption, newInternalConclusion))
      val newSubstitutions = inference.premises.zip(premises.map(_.statement) :+ newInternalPremise)
        .foldLeft(Substitutions.Possible.empty) { case (substitutions, (inferencePremise, premise)) => inferencePremise.calculateSubstitutions(premise, substitutions).getOrElse(throw new Exception("Could not calculate substitutions from naming premise"))}
        .confirmTotality(inference.variableDefinitions)
        .getOrElse(throw new Exception("Naming substitutions were not total"))
      NamingStep(
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
    override def length: Int = substeps.map(_.length).sum + 1
    override def serializedLines: Seq[String] = {
      val labelWords = Seq(
        "let",
        variableName,
        assumption.serialized,
        inference.id,
        substitutions.serialize,
        Premise.serialize(premises))
      val label = labelWords.filter(_.nonEmpty).mkString(" ")
      substeps.flatMap(_.serializedLines).indentInLabelledBracesIfPresent(label)
    }
  }
  object NamingStep {
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[NamingStep] = {
      for {
        variableName <- Parser.singleWord
        assumption <- Statement.parser(ExpressionParsingContext.atStep(stepContext.addBoundVariable(variableName), provingContext))
        innerStepContext = stepContext.addBoundVariable(variableName).addAssumption(assumption)
        inference <- Inference.parser
        substitutions <- inference.substitutionsParser
        premiseStatements = inference.substitutePremises(substitutions).getOrElse(throw new Exception("Could not apply substitutions"))
        premises <- premiseStatements.init.map(Premise.parser).traverse
        substeps <- childStepsParser(innerStepContext, provingContext).inBraces
        internalConclusion = substeps.lastOption.map(_.statement).getOrElse(throw new Exception("No conclusion for naming step"))
        extractedConclusion = internalConclusion.removeExternalParameters(1).getOrElse(throw new Exception("Naming step conclusion could not be extracted"))
        generalizationDefinition = provingContext.generalizationDefinitionOption.getOrElse(throw new Exception("Naming step requires a generalization statement"))
        deductionDefinition = provingContext.deductionDefinitionOption.getOrElse(throw new Exception("Naming step requires a deduction statement"))
        internalPremise = generalizationDefinition(variableName, deductionDefinition(assumption, internalConclusion))
        _ = inference.validatePremisesAndConclusion(premises.map(_.statement) :+ internalPremise, extractedConclusion, substitutions).getOrElse(throw new Exception("Could not validate premises and conclusion"))
      } yield {
        NamingStep(variableName, assumption, extractedConclusion, substeps, inference, premises, substitutions, generalizationDefinition, deductionDefinition)
      }
    }
  }

  case class ElidedStep(substeps: Seq[Step], highlightedInference: Option[Inference.Summary], description: Option[String]) extends Step.WithSubsteps {
    override def statement: Statement = substeps.last.statement
    override def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = copy(substeps = newSubsteps)
    override def length: Int = substeps.map(_.length).sum
    override def serializedLines: Seq[String] = {
      substeps.flatMap(_.serializedLines).indentInLabelledBracesIfPresent(
        ("elided" +: (highlightedInference.map(_.id).toSeq ++ description.map(_.inParens).toSeq)).mkString(" ")
      )
    }
  }
  object ElidedStep {
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[ElidedStep] = {
      for {
        highlightedInference <- Inference.parser.tryOrNone
        description <- Parser.allInParens.tryOrNone
        substeps <- childStepsParser.inBraces
      } yield ElidedStep(substeps, highlightedInference, description)
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
      ifNecessary(substeps, Step.ElidedStep(_, None, Some(description)))
    }
    def ifNecessary(substeps: Seq[Step], inferenceOption: Option[Inference], description: String): Option[Step] = {
      ifNecessary(substeps, inferenceOption.map(forInference).getOrElse(forDescription(description)))
    }

    def forInference(inference: Inference): Seq[Step] => Step.ElidedStep = {
      Step.ElidedStep(_, Some(inference.summary), None)
    }
    def forDescription(description: String): Seq[Step] => Step.ElidedStep = {
      Step.ElidedStep(_, None, Some(description))
    }
  }

  case class SubproofStep(name: String, substeps: Seq[Step]) extends Step.WithSubsteps {
    override def statement: Statement = substeps.last.statement
    override def length: Int = substeps.map(_.length).sum
    override def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = copy(substeps = newSubsteps)
    override def serializedLines: Seq[String] = {
      substeps.flatMap(_.serializedLines).indentInLabelledBracesIfPresent(s"subproof ($name)")
    }
  }
  object SubproofStep {
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[SubproofStep] = {
      for {
        name <- Parser.allInParens
        substeps <- childStepsParser.inBraces
      } yield SubproofStep(name, substeps)
    }
  }

  case class ExistingStatementExtractionStep(extraction: AppliedExtraction) extends Step.PremiseDerivation {
    override def statement: Statement = extraction.statement
    override def length: Int = extraction.length
    override def serializedLines: Seq[String] = extraction.serializedLines.indentInLabelledBracesIfPresent(`type`)
  }
  object ExistingStatementExtractionStep {
    def ifNecessary(substeps: Seq[Step.AssertionStep]): Option[Step.PremiseDerivation] = {
      ifNecessary(AppliedExtraction(substeps.map(AppliedExtractionStep.Assertion(_))))
    }
    def ifNecessary(extraction: AppliedExtraction): Option[Step.PremiseDerivation] = {
      extraction.extractionSteps match {
        case Nil => None
        case Seq(step) => Some(step.toProofStep)
        case _ => Some(ExistingStatementExtractionStep(extraction))
      }
    }
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[ExistingStatementExtractionStep] = {
      for {
        extraction <- AppliedExtraction.parser.inBraces
      } yield ExistingStatementExtractionStep(extraction)
    }
  }

  case class InferenceExtractionStep(assertionStep: Step.AssertionStep, extractionSteps: Seq[Step.AssertionOrExtraction]) extends Step.AssertionOrExtraction with Step.Autogenerated {
    override def inference: Inference.Summary = assertionStep.inference
    override def statement: Statement = extractionSteps.last.statement
    override def substeps: Seq[Step] = assertionStep +: extractionSteps
    override def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = InferenceExtractionStep(newSubsteps)
  }
  object InferenceExtractionStep {
    def apply(appliedInferenceExtraction: AppliedInferenceExtraction): Step.AssertionOrExtraction = {
      if (appliedInferenceExtraction.extraction.extractionSteps.isEmpty) {
        appliedInferenceExtraction.assertionStep
      } else {
        InferenceExtractionStep(
          appliedInferenceExtraction.assertionStep,
          appliedInferenceExtraction.extraction.toProofSteps)
      }
    }
    def apply(steps: Seq[Step]) : InferenceExtractionStep = {
        InferenceExtractionStep(
          steps.head.asInstanceOf[Step.AssertionStep],
          steps.tail.map(_.asInstanceOf[Step.AssertionOrExtraction]))
    }
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[InferenceExtractionStep] = {
      for {
        substeps <- childStepsParser.inBraces
      } yield InferenceExtractionStep(substeps)
    }
  }

  case class WrappedInferenceApplicationStep(
      unwrappers: Seq[Unwrapper],
      premiseSteps: Seq[PremiseDerivation],
      assertionStep: AssertionOrExtraction)
    extends Step.Wrapped
      with Step.InferenceApplicationWithoutPremises
  {
    override def inference: Inference.Summary = assertionStep.inference
    override def innerSteps: Seq[Step] = premiseSteps :+ assertionStep
    override protected def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = {
      WrappedInferenceApplicationStep(newSubsteps)(stepProvingContext.provingContext)
    }
  }
  object WrappedInferenceApplicationStep {
    def apply(steps: Seq[Step])(implicit provingContext: ProvingContext): WrappedInferenceApplicationStep = {
      Wrapped.extractWrappers(steps) { (unwrappers, steps) =>
        WrappedInferenceApplicationStep(
          unwrappers,
          steps.init.map(_.asInstanceOf[PremiseDerivation]),
          steps.last.asInstanceOf[Step.AssertionOrExtraction])
      }
    }
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[WrappedInferenceApplicationStep] = {
      for {
        substeps <- childStepsParser.inBraces
      } yield WrappedInferenceApplicationStep(substeps)
    }
  }

  case class WrappedPremiseDerivationStep(
      unwrappers: Seq[Unwrapper],
      innerSteps: Seq[Step.PremiseDerivation])
    extends Step.Wrapped
      with Step.PremiseDerivation
  {
    override protected def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = {
      WrappedPremiseDerivationStep(newSubsteps)
    }
  }
  object WrappedPremiseDerivationStep {
    def apply(steps: Seq[Step])(implicit provingContext: ProvingContext): WrappedPremiseDerivationStep = {
      Wrapped.extractWrappers(steps) { (unwrappers, steps) =>
        WrappedPremiseDerivationStep(unwrappers, steps.map(_.asInstanceOf[Step.PremiseDerivation]))
      }
    }
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[WrappedPremiseDerivationStep] = {
      for {
        substeps <- childStepsParser.inBraces
      } yield WrappedPremiseDerivationStep(substeps)
    }
  }

  case class InferenceWithPremiseDerivationsStep(
      premiseSteps: Seq[Step.InferenceApplicationWithoutPremises],
      assertionStep: Step.InferenceApplicationWithoutPremises)
    extends Step.InferenceApplication
      with Step.Autogenerated
  {
    override def inference: Inference.Summary = assertionStep.inference
    override def statement: Statement = assertionStep.statement
    override def substeps: Seq[Step] = premiseSteps :+ assertionStep
    override def replaceSubstepsInternal(newSubsteps: Seq[Step])(implicit stepProvingContext: StepProvingContext): Step = InferenceWithPremiseDerivationsStep(newSubsteps)
  }
  object InferenceWithPremiseDerivationsStep {
    def ifNecessary(
      premiseDerivationSteps: Seq[Step.InferenceApplicationWithoutPremises],
      assertion: Step.InferenceApplicationWithoutPremises
    ): Step.InferenceApplication = {
      if (premiseDerivationSteps.nonEmpty) {
        InferenceWithPremiseDerivationsStep(premiseDerivationSteps, assertion)
      } else {
        assertion
      }
    }
    def apply(steps: Seq[Step]): InferenceWithPremiseDerivationsStep = {
      InferenceWithPremiseDerivationsStep(
        steps.init.map(_.asInstanceOf[Step.InferenceApplicationWithoutPremises]),
        steps.last.asInstanceOf[Step.InferenceApplicationWithoutPremises])
    }
    def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[InferenceWithPremiseDerivationsStep] = {
      for {
        substeps <- childStepsParser.inBraces
      } yield InferenceWithPremiseDerivationsStep(substeps)
    }
  }

  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[Option[Step]] = {
    Parser.selectOptionalWordParser {
      case "target" => TargetStep.parser
      case "prove" => AssertionStep.parser
      case "assume" => DeductionStep.parser
      case "take" => GeneralizationStep.parser
      case "let" => NamingStep.parser
      case "elided" => ElidedStep.parser
      case "subproof" => SubproofStep.parser
      case "existingStatementExtraction" => ExistingStatementExtractionStep.parser
      case "inferenceExtraction" => InferenceExtractionStep.parser
      case "wrappedInferenceApplication" => WrappedInferenceApplicationStep.parser
      case "wrappedPremiseDerivation" => WrappedPremiseDerivationStep.parser
      case "inferenceWithPremiseDerivations" => InferenceWithPremiseDerivationsStep.parser
    }
  }
  def childStepsParser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[Seq[Step]] = {
    listParser(stepContext.forChild(), provingContext)
  }
  def listParser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[Seq[Step]] = {
    listParser(parser(_, implicitly)).map(_._1)
  }
  def listParser[T <: Step](getStepParser: StepContext => Parser[Option[T]])(implicit stepContext: StepContext): Parser[(Seq[T], StepContext)] = {
    Parser.mapFoldWhileDefined[T, StepContext](stepContext) { (_, currentStepContext) =>
      getStepParser(currentStepContext)
        .mapMap(step => step -> currentStepContext.addStep(step))
    }
  }
}
