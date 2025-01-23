package net.prover.proving.extraction

import net.prover.model._
import net.prover.model.expressions._
import net.prover.model.proof._
import net.prover.proving.structure.inferences.SpecificationInference
import net.prover.proving.structure.statements.BinaryConnectiveStatement

import scala.annotation.tailrec

object ExtractionCalculator {

  private def getSimpleExtraction(
    extractionSoFar: PartiallyAppliedExtraction,
    inference: Inference,
    extractionPremise: Statement,
    otherPremiseOption: Option[Statement],
    updateExtraction: (PartiallyAppliedExtraction, Step.AssertionStep) => PartiallyAppliedExtraction)(
    implicit substitutionContext: SubstitutionContext
  ): Option[PartiallyAppliedExtraction] = {
    for {
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.confirmTotality(inference.variableDefinitions))
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
      newPremiseOption <- otherPremiseOption.map(_.applySubstitutions(extractionSubstitutions)).swap
      assertionStep = Step.AssertionStep(extractedConclusion, inference.summary, (extractionSoFar.conclusion +: newPremiseOption.toSeq).map(Premise.Pending), extractionSubstitutions)
      newExtraction = updateExtraction(extractionSoFar, assertionStep)
      if !newExtraction.extractionPremises.contains(newExtraction.conclusion) // Filter out spurious extractions
    } yield newExtraction
  }

  private def getStatementExtractions(
    extractionSoFar: PartiallyAppliedExtraction)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[PartiallyAppliedExtraction] = {
    for {
      StatementExtractionInference(inference, extractionPremise, otherPremiseOption) <- provingContext.statementExtractionInferences
      extraction <- getSimpleExtraction(extractionSoFar, inference, extractionPremise, otherPremiseOption, _.appendExtractionStep(_))
    } yield extraction
  }

  private def getPredicateExtraction(
    extractionSoFar: PartiallyAppliedExtraction)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Option[PartiallyAppliedExtraction] = {
    for {
      SpecificationInference(inference, extractionPremise, _) <- provingContext.specificationInferenceOption
      predicate <- extractionPremise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.statements.get(0)) // missing external depth increase?
      boundVariableName <- extractionSoFar.conclusion.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single)
      (_, newIndex, newVariableTracker) = extractionSoFar.variableTracker.getAndAddUniqueVariableName(boundVariableName)
      substitutions = Substitutions(Seq(predicate), Seq(TermVariable(newIndex)))
      newConclusion <- inference.conclusion.applySubstitutions(substitutions)
      assertionStep = Step.AssertionStep(newConclusion, inference.summary, Seq(Premise.Pending(extractionSoFar.conclusion)), substitutions)
    } yield extractionSoFar.appendExtractionStep(assertionStep, Some(newVariableTracker))
  }

  private def getDefinitionDeconstructionExtractions(
    extractionSoFar: PartiallyAppliedExtraction)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[PartiallyAppliedExtraction] = {
    for {
      definedStatement <- extractionSoFar.conclusion.asOptionalInstanceOf[DefinedStatement].toSeq
      definition = definedStatement.definition
      if implicitly[AvailableEntries].typeStatementDefinitions.contains(definition)
      inference <- definedStatement.definition.deconstructionInference.toSeq
      premise <- inference.premises.single.toSeq
      substitutions <- premise.calculateSubstitutions(extractionSoFar.conclusion).flatMap(_.confirmTotality(inference.variableDefinitions)).toSeq
      deconstructedStatement <- inference.conclusion.applySubstitutions(substitutions).toSeq
      assertionStep = Step.AssertionStep(deconstructedStatement, inference.summary, Seq(Premise.Pending(extractionSoFar.conclusion)), substitutions)
    } yield extractionSoFar.appendExtractionStep(assertionStep)
  }

  private def getNextSimplificationExtractions(
    extractionSoFar: PartiallyAppliedExtraction)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[PartiallyAppliedExtraction] = {
    getStatementExtractions(extractionSoFar) ++
      getPredicateExtraction(extractionSoFar).toSeq ++
      getDefinitionDeconstructionExtractions(extractionSoFar)
  }

  private def getSimplificationExtractions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[PartiallyAppliedExtraction] = {
    @tailrec
    def getSimplificationExtractions(newExtractions: Seq[PartiallyAppliedExtraction], oldExtractions: Seq[PartiallyAppliedExtraction]): Seq[PartiallyAppliedExtraction] = {
      if (newExtractions.isEmpty)
        oldExtractions
      else
        getSimplificationExtractions(newExtractions.flatMap(getNextSimplificationExtractions), oldExtractions ++ newExtractions)
    }
    val baseExtraction = PartiallyAppliedExtraction.initial(sourceStatement, variableTracker)
    getSimplificationExtractions(Seq(baseExtraction), Nil)
  }

  private def addReversal(
    extractionSoFar: PartiallyAppliedExtraction)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Option[PartiallyAppliedExtraction] = {
    for {
      reversal <- provingContext.reversals.find(_.joiner.unapply(extractionSoFar.conclusion).nonEmpty)
      premise <- reversal.inference.premises.single
      result <- getSimpleExtraction(extractionSoFar, reversal.inference, premise, None, _.appendReversal(_))
    } yield result
  }

  private def addLeftRewrites(
    extractionSoFar: PartiallyAppliedExtraction)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[PartiallyAppliedExtraction] = {
    for {
      BinaryConnectiveStatement(connective, left, right) <- provingContext.asBinaryConnectiveStatement(extractionSoFar.conclusion).toSeq
      rewrite <- provingContext.chainableRewriteInferences
      transitivity <- rewrite.findValidTransitivity(connective)
      rewriteSubstitutions <- rewrite.right.calculateSubstitutions(left).flatMap(_.confirmTotality(rewrite.inference.variableDefinitions))
      rewrittenLeft <- rewrite.left.applySubstitutions(rewriteSubstitutions)
      assertionStep = Step.AssertionStep(connective(rewrittenLeft, left), rewrite.inference.summary, Nil, rewriteSubstitutions)
      transitivityStep = transitivity.assertionStep(rewrittenLeft, left, right)
    } yield extractionSoFar.appendLeftRewrite(assertionStep, transitivityStep)
  }

  private def addRightRewrites(
    extractionSoFar: PartiallyAppliedExtraction)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[PartiallyAppliedExtraction] = {
    for {
      BinaryConnectiveStatement(connective, left, right) <- provingContext.asBinaryConnectiveStatement(extractionSoFar.conclusion).toSeq
      rewrite <- provingContext.chainableRewriteInferences
      transitivity <- rewrite.findValidTransitivity(connective)
      rewriteSubstitutions <- rewrite.left.calculateSubstitutions(right).flatMap(_.confirmTotality(rewrite.inference.variableDefinitions))
      rewrittenRight <- rewrite.right.applySubstitutions(rewriteSubstitutions)
      assertionStep = Step.AssertionStep(connective(right, rewrittenRight), rewrite.inference.summary, Seq(Premise.Pending(extractionSoFar.conclusion)), rewriteSubstitutions)
      transitivityStep = transitivity.assertionStep(left, right, rewrittenRight)
    } yield extractionSoFar.appendRightRewrite(assertionStep, transitivityStep)
  }

  private def addAll(extractions: Seq[PartiallyAppliedExtraction], getNext: PartiallyAppliedExtraction => IterableOnce[PartiallyAppliedExtraction]): Seq[PartiallyAppliedExtraction] = {
    val newExtractions = extractions.flatMap(getNext)
    extractions ++ newExtractions
  }

  private def getExtractions(
    sourceStatement: Statement,
    variableTracker: VariableTracker,
    getInitial: (Statement, VariableTracker) => Seq[PartiallyAppliedExtraction],
    getNexts: (PartiallyAppliedExtraction => IterableOnce[PartiallyAppliedExtraction])*
  ): Seq[PartiallyAppliedExtraction] = {
    val initial = getInitial(sourceStatement, variableTracker)
    getNexts.foldLeft(initial)(addAll)
  }

  private def getExtractions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[PartiallyAppliedExtraction] = {
    getExtractions(
      sourceStatement,
      variableTracker,
      getSimplificationExtractions,
      addReversal,
      addLeftRewrites,
      addRightRewrites)
  }

  def getInferenceExtractions(inference: Inference)(implicit provingContext: ProvingContext): Seq[InferenceExtraction] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    val statementDefinition = provingContext.availableEntries.statementDefinitions.find(_.constructionInference.contains(inference))
    getExtractions(inference.conclusion, VariableTracker.fromInference(inference))
      .filter(extraction => !inference.premises.contains(extraction.conclusion))
      .filter(extraction => !statementDefinition.exists(_.deconstructionInference.exists(extraction.getDefinition.extractionInferences.contains)))
      .map(innerExtraction => InferenceExtraction(inference.summary, innerExtraction))
  }

  def getPremiseExtractions(premise: Statement)(implicit stepContext: StepContext, provingContext: ProvingContext): Seq[PremiseExtraction] = {
    getExtractions(premise, VariableTracker.fromStepContext)
      .map(innerExtraction => PremiseExtraction(innerExtraction, stepContext))
  }
}
