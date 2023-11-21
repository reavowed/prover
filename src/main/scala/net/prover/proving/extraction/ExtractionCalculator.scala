package net.prover.proving.extraction

import net.prover.model._
import net.prover.model.expressions._
import net.prover.model.proof._

import scala.annotation.tailrec

object ExtractionCalculator {
  trait Extraction {
    def premises: Seq[Statement]
    def conclusion: Statement
    def variableDefinitions: VariableDefinitions
    def extractionDefinition: ExtractionDefinition
    def additionalVariableNames: Seq[String]
  }

  case class ExtractionFromSinglePremise(
    premises: Seq[Statement],
    conclusion: Statement,
    derivation: Seq[Step.Assertion],
    variableTracker: VariableTracker,
    extractionDefinition: ExtractionDefinition)

  case class InferenceExtraction(inference: Inference.Summary, innerExtraction: ExtractionFromSinglePremise) extends Extraction {
    def premises: Seq[Statement] = inference.premises ++ innerExtraction.premises
    def conclusion: Statement = innerExtraction.conclusion
    def variableDefinitions: VariableDefinitions = inference.variableDefinitions.addSimpleTermVariableNames(innerExtraction.variableTracker.additionalVariableNames)
    def extractionDefinition: ExtractionDefinition = innerExtraction.extractionDefinition
    def additionalVariableNames: Seq[String] = innerExtraction.variableTracker.additionalVariableNames
    def derivedSummary: Inference.Summary = Inference.Summary(inference.name, Inference.calculateHash(premises, conclusion), variableDefinitions, premises, conclusion)
  }
  case class PremiseExtraction(innerExtraction: ExtractionFromSinglePremise, stepContext: StepContext) extends Extraction {
    def premises: Seq[Statement] = innerExtraction.premises
    def conclusion: Statement = innerExtraction.conclusion
    def variableDefinitions: VariableDefinitions = stepContext.variableDefinitions.addSimpleTermVariableNames(innerExtraction.variableTracker.additionalVariableNames)
    def extractionDefinition: ExtractionDefinition = innerExtraction.extractionDefinition
    def additionalVariableNames: Seq[String] = innerExtraction.variableTracker.additionalVariableNames
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
    Seq(ExtractionFromSinglePremise(Nil, sourceStatement, Nil, variableTracker, ExtractionDefinition.Empty))
  }

  private def getSimpleExtraction(
    extractionSoFar: ExtractionFromSinglePremise,
    inference: Inference,
    extractionPremise: Statement,
    otherPremiseOption: Option[Statement],
    updateExtractionDefinition: (ExtractionDefinition, Inference.Summary) => ExtractionDefinition)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Option[ExtractionFromSinglePremise] = {
    for {
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.confirmTotality(inference.variableDefinitions))
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
      newPremiseOption <- otherPremiseOption.map(_.applySubstitutions(extractionSubstitutions)).swap
      assertionStep = Step.Assertion(extractedConclusion, inference.summary, (extractionSoFar.conclusion +: newPremiseOption.toSeq).map(Premise.Pending), extractionSubstitutions)
      newExtraction = ExtractionFromSinglePremise(
        extractionSoFar.premises ++ newPremiseOption.toSeq,
        extractedConclusion,
        extractionSoFar.derivation :+ assertionStep,
        extractionSoFar.variableTracker,
        updateExtractionDefinition(extractionSoFar.extractionDefinition, inference.summary))
      if !newExtraction.premises.contains(newExtraction.conclusion) // Filter out spurious extractions
    } yield newExtraction
  }

  private def getStatementExtractions(
    extractionSoFar: ExtractionFromSinglePremise)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    for {
      (inference, extractionPremise, otherPremiseOption) <- provingContext.statementExtractionInferences
      extraction <- getSimpleExtraction(extractionSoFar, inference, extractionPremise, otherPremiseOption, _.addNextExtractionInference(_))
    } yield extraction
  }

  private def getPredicateExtraction(
    extractionSoFar: ExtractionFromSinglePremise)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Option[ExtractionFromSinglePremise] = {
    for {
      (inference, extractionPremise) <- provingContext.specificationInferenceOption
      predicate <- extractionPremise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.statements.get(0)) // missing external depth increase?
      boundVariableName <- extractionSoFar.conclusion.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single)
      (_, newIndex, newVariableTracker) = extractionSoFar.variableTracker.getAndAddUniqueVariableName(boundVariableName)
      substitutions = Substitutions(Seq(predicate), Seq(TermVariable(newIndex)))
      newConclusion <- inference.conclusion.applySubstitutions(substitutions)
      assertionStep = Step.Assertion(newConclusion, inference.summary, Seq(Premise.Pending(extractionSoFar.conclusion)), substitutions)
    } yield ExtractionFromSinglePremise(
      extractionSoFar.premises,
      newConclusion,
      extractionSoFar.derivation :+ assertionStep,
      newVariableTracker,
      extractionSoFar.extractionDefinition.addNextExtractionInference(inference.summary))
  }

  private def getDefinitionDeconstructionExtractions(
    extractionSoFar: ExtractionFromSinglePremise)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    for {
      definedStatement <- extractionSoFar.conclusion.asOptionalInstanceOf[DefinedStatement].toSeq
      definition = definedStatement.definition
      if implicitly[AvailableEntries].typeStatementDefinitions.contains(definition)
      inference <- definedStatement.definition.deconstructionInference.toSeq
      premise <- inference.premises.single.toSeq
      substitutions <- premise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.confirmTotality(inference.variableDefinitions)).toSeq
      deconstructedStatement <- inference.conclusion.applySubstitutions(substitutions).toSeq
      assertionStep = Step.Assertion(deconstructedStatement, inference.summary, Seq(Premise.Pending(extractionSoFar.conclusion)), substitutions)
    } yield ExtractionFromSinglePremise(
      extractionSoFar.premises,
      deconstructedStatement,
      extractionSoFar.derivation :+ assertionStep,
      extractionSoFar.variableTracker,
      extractionSoFar.extractionDefinition.addNextExtractionInference(inference.summary))
  }

  private def getRewrites(
    extractionSoFar: ExtractionFromSinglePremise)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
      provingContext.rewriteInferences.mapCollect { case (inference, firstPremise) =>
        getSimpleExtraction(
          extractionSoFar,
          inference,
          firstPremise,
          None,
          _.setRewriteInference(_))
      }
  }

  private def getNextSimplificationExtractions(
    extractionSoFar: ExtractionFromSinglePremise)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    getStatementExtractions(extractionSoFar) ++
      getPredicateExtraction(extractionSoFar).toSeq ++
      getDefinitionDeconstructionExtractions(extractionSoFar)
  }

  private def getSimplificationExtractions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    @tailrec
    def getSimplificationExtractions(newExtractions: Seq[ExtractionFromSinglePremise], oldExtractions: Seq[ExtractionFromSinglePremise]): Seq[ExtractionFromSinglePremise] = {
      if (newExtractions.isEmpty)
        oldExtractions
      else
        getSimplificationExtractions(newExtractions.flatMap(getNextSimplificationExtractions), oldExtractions ++ newExtractions)
    }
    val baseExtraction = ExtractionFromSinglePremise(
      Nil,
      sourceStatement,
      Nil,
      variableTracker,
      ExtractionDefinition.Empty)
    getSimplificationExtractions(Seq(baseExtraction), Nil)
  }

  private def getExtractions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionFromSinglePremise] = {
    val simplificationExtractions = getSimplificationExtractions(sourceStatement, variableTracker)
    val rewriteExtractions = simplificationExtractions.flatMap(getRewrites(_))
    simplificationExtractions ++ rewriteExtractions
  }

  def getInferenceExtractions(inference: Inference)(implicit provingContext: ProvingContext): Seq[InferenceExtraction] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    getExtractions(inference.conclusion, VariableTracker.fromInference(inference))
      .filter(extraction => !inference.premises.contains(extraction.conclusion))
      .map(innerExtraction => InferenceExtraction(inference.summary, innerExtraction))
  }

  def getPremiseExtractions(premise: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Seq[PremiseExtraction] = {
    getExtractions(premise, VariableTracker.fromStepContext)
      .map(innerExtraction => PremiseExtraction(innerExtraction, stepContext))
  }
}
