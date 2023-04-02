package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object SubstatementExtractor {

  trait Extraction {
    def premises: Seq[Statement]
    def conclusion: Statement
    def variableDefinitions: VariableDefinitions
    def extractionInferences: Seq[Inference.Summary]
    def additionalVariableNames: Seq[String]
  }

  case class ExtractionFromSinglePremise(premises: Seq[Statement], conclusion: Statement, derivation: Seq[DerivationStepWithSingleInference], extractionInferences: Seq[Inference.Summary], additionalVariableNames: Seq[String])

  case class InferenceExtraction(inference: Inference.Summary, innerExtraction: ExtractionFromSinglePremise) extends Extraction {
    def premises: Seq[Statement] = inference.premises ++ innerExtraction.premises
    def conclusion: Statement = innerExtraction.conclusion
    def variableDefinitions: VariableDefinitions = inference.variableDefinitions.addSimpleTermVariableNames(innerExtraction.additionalVariableNames)
    def extractionInferences: Seq[Inference.Summary] = innerExtraction.extractionInferences
    def additionalVariableNames: Seq[String] = innerExtraction.additionalVariableNames
    def derivedSummary: Inference.Summary = Inference.Summary(inference.name, Inference.calculateHash(premises, conclusion), variableDefinitions, premises, conclusion)
  }
  case class PremiseExtraction(innerExtraction: ExtractionFromSinglePremise, stepContext: StepContext) extends Extraction {
    def premises: Seq[Statement] = innerExtraction.premises
    def conclusion: Statement = innerExtraction.conclusion
    def variableDefinitions: VariableDefinitions = stepContext.variableDefinitions.addSimpleTermVariableNames(innerExtraction.additionalVariableNames)
    def extractionInferences: Seq[Inference.Summary] = innerExtraction.extractionInferences
    def additionalVariableNames: Seq[String] = innerExtraction.additionalVariableNames
  }

  case class VariableTracker(baseVariableNames: Seq[String], additionalVariableNames: Seq[String]) {
    def namesUsedSoFar: Seq[String] = baseVariableNames ++ additionalVariableNames
    def getAndAddUniqueVariableName(baseName: String): (String, Int, VariableTracker) = {
      val newName = if (!namesUsedSoFar.contains(baseName))
        baseName
      else {
        val i = Stream.from(1).find(i => !namesUsedSoFar.contains(s"${baseName}_$i")).get
        s"${baseName}_$i"
      }
      (newName, baseVariableNames.length + additionalVariableNames.length, VariableTracker(baseVariableNames, additionalVariableNames :+ newName))
    }
  }
  object VariableTracker {
    def fromInference(inference: Inference): VariableTracker = VariableTracker(inference.variableDefinitions.terms.map(_.name), Nil)
    def fromStepContext(implicit stepContext: StepContext): VariableTracker = VariableTracker(stepContext.variableDefinitions.terms.map(_.name), Nil)
  }

  private def getBaseExtractions(sourceStatement: Statement, variableTracker: VariableTracker): Seq[ExtractionFromSinglePremise] = {
    Seq(ExtractionFromSinglePremise(Nil, sourceStatement, Nil, Nil, variableTracker.additionalVariableNames))
  }

  private def getSimpleExtractions(
    sourceStatement: Statement,
    inference: Inference,
    extractionPremise: Statement,
    otherPremiseOption: Option[Statement],
    recurse: (Statement, VariableTracker) => Seq[ExtractionFromSinglePremise],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    for {
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality(inference.variableDefinitions)).toSeq
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).toSeq
      innerExtraction <- recurse(extractedConclusion, variableTracker)
      newPremiseOption <- otherPremiseOption.map(_.applySubstitutions(extractionSubstitutions)).swap.toSeq
      if !newPremiseOption.contains(innerExtraction.conclusion) // Filter out spurious extractions
      assertionStep = Step.Assertion(extractedConclusion, inference.summary, (newPremiseOption.toSeq :+ sourceStatement).map(Premise.Pending), extractionSubstitutions)
    } yield innerExtraction.copy(
      premises = newPremiseOption.toSeq ++ innerExtraction.premises,
      derivation = DerivationStep.fromAssertion(assertionStep) +: innerExtraction.derivation,
      extractionInferences = inference.summary +: innerExtraction.extractionInferences)
  }

  private def getSimpleExtractions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionFromSinglePremise],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    for {
      (inference, extractionPremise, otherPremiseOption) <- provingContext.statementExtractionInferences
      extraction <- getSimpleExtractions(sourceStatement, inference, extractionPremise, otherPremiseOption, recurse, variableTracker)
    } yield extraction
  }

  private def getPredicateExtractions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionFromSinglePremise],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    for {
      (inference, extractionPremise) <- provingContext.specificationInferenceOption.toSeq
      predicate <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.statements.get(0)).toSeq // missing external depth increase?
      boundVariableName <- sourceStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).toSeq
      (_, newIndex, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      substitutions = Substitutions(Seq(predicate), Seq(TermVariable(newIndex)))
      nextPremise <- inference.conclusion.applySubstitutions(substitutions).toSeq
      innerExtraction <- recurse(nextPremise, newVariableTracker)
      assertionStep = Step.Assertion(nextPremise, inference.summary, Seq(Premise.Pending(sourceStatement)), substitutions)
    } yield innerExtraction.copy(
      derivation = DerivationStep.fromAssertion(assertionStep) +: innerExtraction.derivation,
      extractionInferences = inference.summary +: innerExtraction.extractionInferences)
  }

  private def getDefinitionDeconstructionExtractions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionFromSinglePremise],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    for {
      definedStatement <- sourceStatement.asOptionalInstanceOf[DefinedStatement].toSeq
      definition = definedStatement.definition
      if implicitly[AvailableEntries].typeStatementDefinitions.contains(definition)
      inference <- definedStatement.definition.deconstructionInference.toSeq
      premise <- inference.premises.single.toSeq
      substitutions <- premise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality(inference.variableDefinitions)).toSeq
      deconstructedStatement <- inference.conclusion.applySubstitutions(substitutions).toSeq
      innerExtraction <- recurse(deconstructedStatement, variableTracker)
      assertionStep = Step.Assertion(deconstructedStatement, inference.summary, Seq(Premise.Pending(sourceStatement)), substitutions)
    } yield innerExtraction.copy(
      derivation = DerivationStep.fromAssertion(assertionStep) +: innerExtraction.derivation,
      extractionInferences = inference.summary +: innerExtraction.extractionInferences)
  }

  private def getFinalExtractions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    getBaseExtractions(sourceStatement, variableTracker) ++
      provingContext.rewriteInferences.flatMap { case (inference, firstPremise) =>
        getSimpleExtractions(
          sourceStatement,
          inference,
          firstPremise,
          None,
          getBaseExtractions,
          variableTracker)
      }
  }

  private def getExtractionsRecursive(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    getFinalExtractions(sourceStatement, variableTracker) ++
      getSimpleExtractions(sourceStatement, getExtractionsRecursive, variableTracker) ++
      getPredicateExtractions(sourceStatement, getExtractionsRecursive, variableTracker) ++
      getDefinitionDeconstructionExtractions(sourceStatement, getExtractionsRecursive, variableTracker)
  }

  private def getExtractions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    getExtractionsRecursive(sourceStatement, variableTracker)
  }

  def getInferenceExtractions(inference: Inference)(implicit provingContext: ProvingContext): Seq[InferenceExtraction] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    getExtractions(inference.conclusion, VariableTracker.fromInference(inference))
      .filter(extraction => !inference.premises.contains(extraction.conclusion))
      .map(innerExtraction => InferenceExtraction(inference.summary, innerExtraction))
  }

  def getPremiseExtractions(premise: Statement)(implicit stepContext: StepContext): Seq[PremiseExtraction] = {
    getExtractions(premise, VariableTracker.fromStepContext)
      .map(innerExtraction => PremiseExtraction(innerExtraction, stepContext))
  }

  def createDerivationForInferenceExtraction(
    assertionStep: Step.Assertion,
    derivationSteps: Seq[DerivationStep])(
    implicit provingContext: ProvingContext
  ): DerivationStepWithSingleInference = {
    val updatedSteps = groupStepsByDefinition(derivationSteps, Some(DerivationStep.fromAssertion(assertionStep)))
    updatedSteps.elideWithInference(assertionStep.inference)
  }

  def groupStepsByDefinition(steps: Seq[DerivationStep], initialMainStep: Option[DerivationStepWithSingleInference])(implicit provingContext: ProvingContext): Seq[DerivationStep] = {
    val deconstructionInferenceIds = provingContext.availableEntries.statementDefinitions.mapCollect(_.deconstructionInference).map(_.id).toSet
    val structuralSimplificationIds = provingContext.structuralSimplificationInferences.map(_._1.id).toSet

    var currentMainStep: Option[DerivationStepWithSingleInference] = initialMainStep
    val currentUngroupedSteps = Seq.newBuilder[DerivationStep]
    val stepsToReturn = Seq.newBuilder[DerivationStep]

    def isStructuralSimplification(step: DerivationStep): Boolean = {
      step.asOptionalInstanceOf[DerivationStepWithSingleInference].exists(s => structuralSimplificationIds.contains(s.inference.id))
    }

    def removeStructuralSimplifications(steps: Seq[DerivationStep]): Seq[DerivationStep] = {
      steps.filter(s => !isStructuralSimplification(s))
    }
    def removeNonEndStructuralSimplifications(steps: Seq[DerivationStep]): Seq[DerivationStep] = {
      @scala.annotation.tailrec
      def whileStructuralAtEnd(current: Seq[DerivationStep], end: Seq[DerivationStep]): Seq[DerivationStep] = {
        current match {
          case init :+ last if isStructuralSimplification(last) =>
            whileStructuralAtEnd(init, last +: end)
          case _ =>
            removeStructuralSimplifications(current) ++ end
        }
      }
      whileStructuralAtEnd(steps, Nil)
    }
    def groupSteps(steps: Seq[DerivationStep], retainEndSimplifications: Boolean): Unit = {
      currentMainStep match {
        case Some(step) =>
          stepsToReturn += step.elideWithFollowingSteps(removeNonEndStructuralSimplifications(steps))
        case None =>
          stepsToReturn ++= (if (retainEndSimplifications) removeNonEndStructuralSimplifications(steps) else removeStructuralSimplifications(steps))
      }
    }

    for (currentStep <- steps) {
      currentStep match {
        case deconstructionStep @ DerivationStepWithSingleInference(_, inference, _) if deconstructionInferenceIds.contains(inference.id) =>
          groupSteps(currentUngroupedSteps.result(), false)
          currentMainStep = Some(deconstructionStep)
          currentUngroupedSteps.clear()
        case _ =>
          currentUngroupedSteps += currentStep
      }
    }
    groupSteps(currentUngroupedSteps.result(), true)
    stepsToReturn.result()
  }
}
