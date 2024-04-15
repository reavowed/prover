package net.prover.proving.extraction

import net.prover.model._
import net.prover.model.expressions._
import net.prover.model.proof._

import scala.annotation.tailrec

object ExtractionCalculator {

  private def getBaseExtractions(sourceStatement: Statement, variableTracker: VariableTracker): Seq[ExtractionDetails] = {
    Seq(ExtractionDetails(Nil, sourceStatement, Nil, variableTracker, ExtractionDefinition.Empty))
  }

  private def getSimpleExtraction(
    extractionSoFar: ExtractionDetails,
    inference: Inference,
    extractionPremise: Statement,
    otherPremiseOption: Option[Statement],
    updateExtractionDefinition: (ExtractionDefinition, Inference.Summary) => ExtractionDefinition)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Option[ExtractionDetails] = {
    for {
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.confirmTotality(inference.variableDefinitions))
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
      newPremiseOption <- otherPremiseOption.map(_.applySubstitutions(extractionSubstitutions)).swap
      assertionStep = Step.AssertionStep(extractedConclusion, inference.summary, (extractionSoFar.conclusion +: newPremiseOption.toSeq).map(Premise.Pending), extractionSubstitutions)
      newExtraction = ExtractionDetails(
        extractionSoFar.extractionPremises ++ newPremiseOption.toSeq,
        extractedConclusion,
        extractionSoFar.derivation :+ assertionStep,
        extractionSoFar.variableTracker,
        updateExtractionDefinition(extractionSoFar.extractionDefinition, inference.summary))
      if !newExtraction.extractionPremises.contains(newExtraction.conclusion) // Filter out spurious extractions
    } yield newExtraction
  }

  private def getStatementExtractions(
    extractionSoFar: ExtractionDetails)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionDetails] = {
    for {
      (inference, extractionPremise, otherPremiseOption) <- provingContext.statementExtractionInferences
      extraction <- getSimpleExtraction(extractionSoFar, inference, extractionPremise, otherPremiseOption, _.addNextExtractionInference(_))
    } yield extraction
  }

  private def getPredicateExtraction(
    extractionSoFar: ExtractionDetails)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Option[ExtractionDetails] = {
    for {
      (inference, extractionPremise) <- provingContext.specificationInferenceOption
      predicate <- extractionPremise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.statements.get(0)) // missing external depth increase?
      boundVariableName <- extractionSoFar.conclusion.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single)
      (_, newIndex, newVariableTracker) = extractionSoFar.variableTracker.getAndAddUniqueVariableName(boundVariableName)
      substitutions = Substitutions(Seq(predicate), Seq(TermVariable(newIndex)))
      newConclusion <- inference.conclusion.applySubstitutions(substitutions)
      assertionStep = Step.AssertionStep(newConclusion, inference.summary, Seq(Premise.Pending(extractionSoFar.conclusion)), substitutions)
    } yield ExtractionDetails(
      extractionSoFar.extractionPremises,
      newConclusion,
      extractionSoFar.derivation :+ assertionStep,
      newVariableTracker,
      extractionSoFar.extractionDefinition.addNextExtractionInference(inference.summary))
  }

  private def getDefinitionDeconstructionExtractions(
    extractionSoFar: ExtractionDetails)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionDetails] = {
    for {
      definedStatement <- extractionSoFar.conclusion.asOptionalInstanceOf[DefinedStatement].toSeq
      definition = definedStatement.definition
      if implicitly[AvailableEntries].typeStatementDefinitions.contains(definition)
      inference <- definedStatement.definition.deconstructionInference.toSeq
      premise <- inference.premises.single.toSeq
      substitutions <- premise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.confirmTotality(inference.variableDefinitions)).toSeq
      deconstructedStatement <- inference.conclusion.applySubstitutions(substitutions).toSeq
      assertionStep = Step.AssertionStep(deconstructedStatement, inference.summary, Seq(Premise.Pending(extractionSoFar.conclusion)), substitutions)
    } yield ExtractionDetails(
      extractionSoFar.extractionPremises,
      deconstructedStatement,
      extractionSoFar.derivation :+ assertionStep,
      extractionSoFar.variableTracker,
      extractionSoFar.extractionDefinition.addNextExtractionInference(inference.summary))
  }

  private def getReversal(
    extractionSoFar: ExtractionDetails)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Option[ExtractionDetails] = {
    for {
      reversal <- provingContext.reversals.find(_.joiner.unapply(extractionSoFar.conclusion).nonEmpty)
      premise <- reversal.inference.premises.single
      result <- getSimpleExtraction(extractionSoFar, reversal.inference, premise, None, _.setReversalInference(_))
    } yield result
  }

  private def getNextSimplificationExtractions(
    extractionSoFar: ExtractionDetails)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionDetails] = {
    getStatementExtractions(extractionSoFar) ++
      getPredicateExtraction(extractionSoFar).toSeq ++
      getDefinitionDeconstructionExtractions(extractionSoFar)
  }

  private def getSimplificationExtractions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionDetails] = {
    @tailrec
    def getSimplificationExtractions(newExtractions: Seq[ExtractionDetails], oldExtractions: Seq[ExtractionDetails]): Seq[ExtractionDetails] = {
      if (newExtractions.isEmpty)
        oldExtractions
      else
        getSimplificationExtractions(newExtractions.flatMap(getNextSimplificationExtractions), oldExtractions ++ newExtractions)
    }
    val baseExtraction = ExtractionDetails(
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
  ): Seq[ExtractionDetails] = {
    val simplificationExtractions = getSimplificationExtractions(sourceStatement, variableTracker)
    val rewriteExtractions = simplificationExtractions.flatMap(getReversal(_))
    simplificationExtractions ++ rewriteExtractions
  }

  def getInferenceExtractions(inference: Inference)(implicit provingContext: ProvingContext): Seq[InferenceExtraction] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    val statementDefinition = provingContext.availableEntries.statementDefinitions.find(_.constructionInference.contains(inference))
    getExtractions(inference.conclusion, VariableTracker.fromInference(inference))
      .filter(extraction => !inference.premises.contains(extraction.conclusion))
      .filter(extraction => !statementDefinition.exists(_.deconstructionInference.exists(extraction.extractionDefinition.extractionInferences.contains)))
      .map(innerExtraction => InferenceExtraction(inference.summary, innerExtraction))
  }

  def getPremiseExtractions(premise: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Seq[PremiseExtraction] = {
    getExtractions(premise, VariableTracker.fromStepContext)
      .map(innerExtraction => PremiseExtraction(innerExtraction, stepContext))
  }
}
