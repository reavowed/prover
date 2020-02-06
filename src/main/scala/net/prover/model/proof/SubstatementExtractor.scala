package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object SubstatementExtractor {

  case class ExtractionOption(extractionResult: Statement, premises: Seq[Statement], inferences: Seq[Inference])

  case class ExtractionResult(extractionSteps: Seq[Step], targetSteps: Seq[Step.Target], terms: Map[Int, Term])

  case class VariableTracker(namesUsedSoFar: Seq[String]) {
    def getAndAddUniqueVariableName(baseName: String): (String, VariableTracker) = {
      val newName = if (!namesUsedSoFar.contains(baseName))
        baseName
      else {
        val i = Stream.from(1).find(i => !namesUsedSoFar.contains(s"${baseName}_$i")).get
        s"${baseName}_$i"
      }
      (newName, VariableTracker(namesUsedSoFar :+ newName))
    }
  }
  object VariableTracker {
    def fromInference(inference: Inference): VariableTracker = VariableTracker(inference.requiredSubstitutions.terms.map(_._1))
    def fromStepContext(implicit stepContext: StepContext): VariableTracker = VariableTracker(stepContext.termVariableNames)
  }


  private def getBaseExtractionOption(sourceStatement: Statement): Seq[ExtractionOption] = {
    Seq(ExtractionOption(sourceStatement, Nil, Nil))
  }

  private def getStatementExtractionOptions(
    sourceStatement: Statement,
    inference: Inference,
    extractionPremise: Statement,
    otherPremiseOption: Option[Statement],
    recurse: (Statement, VariableTracker) => Seq[ExtractionOption],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    for {
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality).toSeq
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).toSeq
      innerOption <- recurse(extractedConclusion, variableTracker)
      newPremiseOption <- otherPremiseOption.map(_.applySubstitutions(extractionSubstitutions)).swap.toSeq
      if !newPremiseOption.contains(innerOption.extractionResult) // Filter out spurious extractions
    } yield innerOption.copy(premises = newPremiseOption.toSeq ++ innerOption.premises, inferences = inference +: innerOption.inferences)
  }

  private def getStatementExtractionOptions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionOption],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    for {
      (inference, extractionPremise, otherPremiseOption) <- provingContext.statementExtractionInferences
      extractionOption <- getStatementExtractionOptions(sourceStatement, inference, extractionPremise, otherPremiseOption, recurse, variableTracker)
    } yield extractionOption
  }

  private def getPredicateExtractionOptions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionOption],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    for {
      (inference, extractionPremise, predicateName, _) <- provingContext.specificationInferenceOption.toSeq
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality).toSeq // missing external depth increase?
      boundVariableName <- sourceStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.scopedBoundVariableNames.single).toSeq
      (1, extractionPredicate) <- extractionSubstitutions.statements.get(predicateName).toSeq
      (newVariableName, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      nextPremise <- extractionPredicate.specify(Seq(TermVariable(newVariableName))).toSeq
      innerOption <- recurse(nextPremise, newVariableTracker)
    } yield innerOption.copy(inferences = inference +: innerOption.inferences)
  }

  private def getDefinitionDeconstructionExtractionOptions(
    sourceStatement: Statement,
    recurse: (Statement, VariableTracker) => Seq[ExtractionOption],
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    for {
      definedStatement <- sourceStatement.asOptionalInstanceOf[DefinedStatement].toSeq
      definition = definedStatement.definition
      if (
        implicitly[EntryContext].typeDefinitions.map(_.statementDefinition) ++
        implicitly[EntryContext].propertyDefinitionsByType.values.flatten.map(_.statementDefinition)
      ).contains(definition)
      deconstructionInference <- definedStatement.definition.deconstructionInference.toSeq
      extractionPremise <- deconstructionInference.premises.single.toSeq
      extractedSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality).toSeq
      deconstructedStatement <- deconstructionInference.conclusion.applySubstitutions(extractedSubstitutions).toSeq
      innerOption <- recurse(deconstructedStatement, variableTracker)
    } yield innerOption.copy(inferences = deconstructionInference +: innerOption.inferences)
  }

  private def getFinalExtractionOptions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    getBaseExtractionOption(sourceStatement) ++
      provingContext.rewriteInferences.flatMap { case (inference, firstPremise) =>
        getStatementExtractionOptions(
          sourceStatement,
          inference,
          firstPremise,
          None,
          (s, _) => getBaseExtractionOption(s),
          variableTracker)
      }
  }

  private def getExtractionOptions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    getFinalExtractionOptions(sourceStatement, variableTracker) ++
      getStatementExtractionOptions(sourceStatement, getExtractionOptions, variableTracker) ++
      getPredicateExtractionOptions(sourceStatement, getExtractionOptions, variableTracker) ++
      getDefinitionDeconstructionExtractionOptions(sourceStatement, getExtractionOptions, variableTracker)
  }

  def getExtractionOptions(inference: Inference)(implicit provingContext: ProvingContext): Seq[ExtractionOption] = {
    implicit val substitutionContext = SubstitutionContext.outsideProof
    getExtractionOptions(inference.conclusion, VariableTracker.fromInference(inference)).filter(extractionOption => !inference.premises.contains(extractionOption.extractionResult))
  }

  def getExtractionOptions(premise: Statement)(implicit stepProvingContext: StepProvingContext): Seq[ExtractionOption] = {
    getExtractionOptions(premise, VariableTracker.fromStepContext)
  }

  private def findByExtractingBase(
    sourceStatement: Statement,
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[ExtractionResult] = {
    sourceStatement.calculateArguments(targetStatement, Map.empty)
      .map(terms => ExtractionResult(Nil, Nil, terms))
      .toSeq
  }

  private def findByExtractingStatement(
    sourceStatement: Statement,
    targetStatement: Statement,
    termsUsed: Int,
    recurse: (Statement, Statement, Int) => Seq[ExtractionResult],
    inference: Inference,
    extractionPremise: Statement,
    otherPremiseOption: Option[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[ExtractionResult] = {
    for {
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement)(StepContext.withExtraParameter).flatMap(_.confirmTotality).toSeq
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)(StepContext.withExtraParameter).toSeq
      ExtractionResult(innerExtractionSteps, innerTargetSteps, innerTerms) <- recurse(extractedConclusion, targetStatement, termsUsed)
      extractedOtherPremiseOption <- otherPremiseOption.map(_.applySubstitutions(extractionSubstitutions)(StepContext.withExtraParameter)).swap.toSeq
      (premiseSteps, targetSteps, terms) <- extractedOtherPremiseOption match {
        case Some(extractedOtherPremise) =>
          val results = PremiseFinder.findParameterisedPremiseSteps(extractedOtherPremise, innerTerms)
          if (results.nonEmpty)
            results.map { case (steps, terms) => (steps, Nil, terms) }
          else
            extractedOtherPremise.specify(innerTerms).map(target => (Nil, Seq(Step.Target(target)), innerTerms)).toSeq
        case None =>
          Seq((Nil, Nil, innerTerms))
      }
      substitutedExtractionPremise <- sourceStatement.specify(terms)
      substitutions <- extractionPremise.calculateSubstitutions(substitutedExtractionPremise).flatMap(_.confirmTotality)
      substitutedOtherPremiseOption <- otherPremiseOption.map(_.applySubstitutions(substitutions)).swap
      substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions)
      assertionStep = Step.Assertion(
        substitutedConclusion,
        inference.summary,
        (substitutedExtractionPremise +: substitutedOtherPremiseOption.toSeq).map(Premise.Pending),
        substitutions)
      extractionStep = Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference).get
    } yield ExtractionResult(extractionStep +: innerExtractionSteps, targetSteps ++ innerTargetSteps, terms)
  }

  private def findByRewriting(
    sourceStatement: Statement,
    targetStatement: Statement,
    termsUsed: Int)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[ExtractionResult] = {
    stepProvingContext.provingContext.rewriteInferences.flatMap { case (inference, rewritePremise) =>
      findByExtractingStatement(
        sourceStatement,
        targetStatement,
        termsUsed,
        (s, t, _) => findByExtractingBase(s, t),
        inference,
        rewritePremise,
        None)
    }
  }

  private def findByExtractingStatement(
    sourceStatement: Statement,
    targetStatement: Statement,
    termsUsed: Int)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[ExtractionResult] = {
    implicitly[ProvingContext].statementExtractionInferences.flatMap { case (inference, extractionPremise, otherPremiseOption) =>
      findByExtractingStatement(sourceStatement, targetStatement, termsUsed, findByExtracting, inference, extractionPremise, otherPremiseOption)
    }
  }

  private def findByExtractingPredicate(
    sourceStatement: Statement,
    targetStatement: Statement,
    termsUsed: Int)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[ExtractionResult] = {
    implicitly[ProvingContext].specificationInferenceOption.toSeq.flatMap { case (inference, extractionPremise, predicateName, variableName) =>
      for {
        extractionSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement)(StepContext.withExtraParameter).flatMap(_.confirmTotality).toSeq
        (1, extractionPredicate) <- extractionSubstitutions.statements.get(predicateName).toSeq
        nextPremise <- extractionPredicate.specify(Seq(FunctionParameter(termsUsed, stepProvingContext.stepContext.externalDepth)))(StepContext.withExtraParameter).toSeq
        ExtractionResult(extractionSteps, innerTargetSteps, terms) <- findByExtracting(nextPremise, targetStatement, termsUsed + 1)
        specificationArgument <- terms.get(termsUsed).toSeq
        substitutedPremise <- sourceStatement.specify(terms).toSeq
        substitutions <- extractionPremise.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality)
          .map(_.copy(terms = Map(variableName -> (0, specificationArgument))))
          .toSeq
        substitutedConclusion <- inference.conclusion.applySubstitutions(substitutions).toSeq
        assertionStep = Step.Assertion(
          substitutedConclusion,
          inference.summary,
          Seq(Premise.Pending(substitutedPremise)),
          substitutions)
      } yield ExtractionResult(assertionStep +: extractionSteps, innerTargetSteps, terms)
    }
  }

  private def findByExtractingDefinition(
    sourceStatement: Statement,
    targetStatement: Statement,
    termsUsed: Int)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[ExtractionResult] = {
    for {
      definedStatement <- sourceStatement.asOptionalInstanceOf[DefinedStatement].toSeq
      definition = definedStatement.definition
      if (
        implicitly[EntryContext].typeDefinitions.map(_.statementDefinition) ++
        implicitly[EntryContext].propertyDefinitionsByType.values.flatten.map(_.statementDefinition)
      ).contains(definition)
      deconstructionInference <- definedStatement.definition.deconstructionInference.toSeq
      premise <- deconstructionInference.premises.single.toSeq
      extractionSubstitutions <- premise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality).toSeq
      deconstructedStatement <- deconstructionInference.conclusion.applySubstitutions(extractionSubstitutions).iterator
      ExtractionResult(extractionSteps, innerTargetSteps, terms) <- findByExtracting(deconstructedStatement, targetStatement, termsUsed)
      substitutedPremise <- sourceStatement.specify(terms).iterator
      substitutions <- premise.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality).iterator
      substitutedConclusion <- deconstructionInference.conclusion.applySubstitutions(substitutions).iterator
      assertionStep = Step.Assertion(
        substitutedConclusion,
        deconstructionInference.summary,
        Seq(Premise.Pending(substitutedPremise)),
        substitutions)
    } yield ExtractionResult(assertionStep +: extractionSteps, innerTargetSteps, terms)
  }

  private def findByExtracting(
    sourceStatement: Statement,
    targetStatement: Statement,
    termsUsed: Int)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[ExtractionResult] = {
    findByExtractingBase(sourceStatement, targetStatement) ++
      findByRewriting(sourceStatement, targetStatement, termsUsed) ++
      findByExtractingStatement(sourceStatement, targetStatement, termsUsed) ++
      findByExtractingPredicate(sourceStatement, targetStatement, termsUsed) ++
      findByExtractingDefinition(sourceStatement, targetStatement, termsUsed)
  }

  def findByExtracting(sourceStatement: Statement, targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Seq[(Step, Seq[Step.Target])] = {
    findByExtracting(sourceStatement, targetStatement, 0).mapCollect { case ExtractionResult(extractionSteps, targetSteps, _) =>
      Step.Elided.ifNecessary(extractionSteps, "Extracted").map((_, targetSteps))
    }
  }
}
