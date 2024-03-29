package net.prover.proving.extraction

import net.prover.model._
import net.prover.model.expressions._
import net.prover.model.proof._

object ExtractionCalculator {
  trait Extraction {
    def premises: Seq[Statement]
    def conclusion: Statement
    def variableDefinitions: VariableDefinitions
    def extractionInferences: Seq[Inference.Summary]
    def additionalVariableNames: Seq[String]
  }

  case class ExtractionFromSinglePremise(
    premises: Seq[Statement],
    conclusion: Statement,
    derivation: Seq[Step.Assertion],
    extractionInferences: Seq[Inference.Summary],
    additionalVariableNames: Seq[String])

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
      derivation = assertionStep +: innerExtraction.derivation,
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
      derivation = assertionStep +: innerExtraction.derivation,
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
      derivation = assertionStep +: innerExtraction.derivation,
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

  def getPremiseExtractions(premise: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Seq[PremiseExtraction] = {
    getExtractions(premise, VariableTracker.fromStepContext)
      .map(innerExtraction => PremiseExtraction(innerExtraction, stepContext))
  }
}
