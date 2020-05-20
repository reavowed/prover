package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object SubstatementExtractor {

  case class ExtractionOption(premises: Seq[Statement], conclusion: Statement, derivation: Seq[DerivationStep], extractionInferences: Seq[Inference], additionalVariableNames: Seq[String]) {
    def requiredSubstitutions: Substitutions.Required = (premises.map(_.requiredSubstitutions) :+ conclusion.requiredSubstitutions).foldTogether
  }

  case class VariableTracker(baseVariableNames: Seq[String], additionalVariableNames: Seq[String]) {
    def namesUsedSoFar: Seq[String] = baseVariableNames ++ additionalVariableNames
    def getAndAddUniqueVariableName(baseName: String): (String, VariableTracker) = {
      val newName = if (!namesUsedSoFar.contains(baseName))
        baseName
      else {
        val i = Stream.from(1).find(i => !namesUsedSoFar.contains(s"${baseName}_$i")).get
        s"${baseName}_$i"
      }
      (newName, VariableTracker(baseVariableNames, additionalVariableNames :+ newName))
    }
  }
  object VariableTracker {
    def fromInference(inference: Inference): VariableTracker = VariableTracker(inference.requiredSubstitutions.terms.map(_._1), Nil)
    def fromStepContext(implicit stepContext: StepContext): VariableTracker = VariableTracker(stepContext.termVariableNames, Nil)
  }

  private def getBaseExtractionOption(sourceStatement: Statement, variableTracker: VariableTracker): Seq[ExtractionOption] = {
    Seq(ExtractionOption(Nil, sourceStatement, Nil, Nil, variableTracker.additionalVariableNames))
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
      if !newPremiseOption.contains(innerOption.conclusion) // Filter out spurious extractions
      assertionStep = Step.Assertion(extractedConclusion, inference.summary, (newPremiseOption.toSeq :+ sourceStatement).map(Premise.Pending), extractionSubstitutions)
    } yield innerOption.copy(
      premises = newPremiseOption.toSeq ++ innerOption.premises,
      derivation = DerivationStep.fromAssertion(assertionStep) +: innerOption.derivation,
      extractionInferences = inference +: innerOption.extractionInferences)
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
      (inference, extractionPremise, _, specificationVariableName) <- provingContext.specificationInferenceOption.toSeq
      substitutionsFromPremise <- extractionPremise.calculateSubstitutions(sourceStatement).toSeq // missing external depth increase?
      boundVariableName <- sourceStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).toSeq
      (newVariableName, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      fullSubstitutions <- substitutionsFromPremise.update(specificationVariableName, 0, TermVariable(newVariableName), Substitutions.Possible.termsLens).flatMap(_.confirmTotality).toSeq
      nextPremise <- inference.conclusion.applySubstitutions(fullSubstitutions).toSeq
      innerOption <- recurse(nextPremise, newVariableTracker)
      assertionStep = Step.Assertion(nextPremise, inference.summary, Seq(Premise.Pending(sourceStatement)), fullSubstitutions)
    } yield innerOption.copy(
      derivation = DerivationStep.fromAssertion(assertionStep) +: innerOption.derivation,
      extractionInferences = inference +: innerOption.extractionInferences)
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
      if implicitly[EntryContext].typeStatementDefinitions.contains(definition)
      deconstructionInference <- definedStatement.definition.deconstructionInference.toSeq
      extractionPremise <- deconstructionInference.premises.single.toSeq
      extractedSubstitutions <- extractionPremise.calculateSubstitutions(sourceStatement).flatMap(_.confirmTotality).toSeq
      deconstructedStatement <- deconstructionInference.conclusion.applySubstitutions(extractedSubstitutions).toSeq
      innerOption <- recurse(deconstructedStatement, variableTracker)
      assertionStep = Step.Assertion(deconstructedStatement, deconstructionInference.summary, Seq(Premise.Pending(sourceStatement)), extractedSubstitutions)
    } yield innerOption.copy(
      derivation = DerivationStep.fromAssertion(assertionStep) +: innerOption.derivation,
      extractionInferences = deconstructionInference +: innerOption.extractionInferences)
  }

  private def getFinalExtractionOptions(
    sourceStatement: Statement,
    variableTracker: VariableTracker)(
    implicit substitutionContext: SubstitutionContext,
    provingContext: ProvingContext
  ): Seq[ExtractionOption] = {
    getBaseExtractionOption(sourceStatement, variableTracker) ++
      provingContext.rewriteInferences.flatMap { case (inference, firstPremise) =>
        getStatementExtractionOptions(
          sourceStatement,
          inference,
          firstPremise,
          None,
          getBaseExtractionOption,
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
    getExtractionOptions(inference.conclusion, VariableTracker.fromInference(inference))
      .filter(extractionOption => !inference.premises.contains(extractionOption.conclusion))
      .map(extractionOption => extractionOption.copy(premises = inference.premises ++ extractionOption.premises))
  }

  def getExtractionOptions(premise: Statement)(implicit stepProvingContext: StepProvingContext): Seq[ExtractionOption] = {
    getExtractionOptions(premise, VariableTracker.fromStepContext)
  }

  def createDerivationForInferenceExtraction(
    assertionStep: Step.Assertion,
    derivationSteps: Seq[DerivationStep])(
    implicit provingContext: ProvingContext
  ): DerivationStep = {
    val updatedSteps = groupStepsByDefinition(derivationSteps, Some(DerivationStep.fromAssertion(assertionStep)))
    updatedSteps.head.elideWithFollowingSteps(updatedSteps.tail)
  }

  def groupStepsByDefinition(steps: Seq[DerivationStep], initialMainStep: Option[DerivationStep])(implicit provingContext: ProvingContext): Seq[DerivationStep] = {
    val structuralSimplificationIds = provingContext.structuralSimplificationInferences.map(_._1.id).toSet

    var currentMainStep: Option[DerivationStep] = initialMainStep
    val currentUngroupedSteps = Seq.newBuilder[DerivationStep]
    val stepsToReturn = Seq.newBuilder[DerivationStep]

    def removeUnnecessarySimplifications(steps: Seq[DerivationStep], typeSymbol: String): Seq[DerivationStep] = {
      val definitions = provingContext.entryContext.typeStatementDefinitionsByType(typeSymbol)
      def isTypeStatement(statement: Statement): Boolean = {
        statement.asOptionalInstanceOf[DefinedStatement].map(_.definition).contains(provingContext.entryContext.typeDefinitions(typeSymbol).statementDefinition)
      }
      def isAccompanyingStatement(statement: Statement): Boolean = {
        statement.asOptionalInstanceOf[DefinedStatement].map(_.definition).exists(definitions.contains)
      }
      def isCombinedTypeStatement(statement: Statement): Boolean = {
        isTypeStatement(statement) ||
          provingContext.entryContext.conjunctionDefinitionOption.exists { conjunction =>
            conjunction.unapply(statement).exists { case (a, b) => isCombinedTypeStatement(a) && isAccompanyingStatement(b) }
          }
      }
      @scala.annotation.tailrec
      def helper(cleared: Seq[DerivationStep], remaining: Seq[DerivationStep]): Seq[DerivationStep] = {
        remaining match {
          case Nil =>
            cleared
          case head +: tail =>
            if (isCombinedTypeStatement(head.statement) && tail.forall(s => structuralSimplificationIds.contains(s.inference.id)))
              cleared :+ head
            else
              helper(cleared :+ head, tail)
        }
      }
      helper(Nil, steps)
    }
    def removeNonEndStructuralSimplifications(steps: Seq[DerivationStep]): Seq[DerivationStep] = {
      @scala.annotation.tailrec
      def helper(remainingAssertions: Seq[DerivationStep], filteredAssertions: Seq[DerivationStep]): Seq[DerivationStep] = {
        remainingAssertions match {
          case head +: tail if structuralSimplificationIds.contains(head.inference.id) && tail.exists(a => !structuralSimplificationIds.contains(a.inference.id)) =>
            helper(tail, filteredAssertions)
          case head +: tail =>
            helper(tail, filteredAssertions :+ head)
          case Nil =>
            filteredAssertions
        }
      }
      helper(steps, Nil)
    }
    def groupSteps(steps: Seq[DerivationStep]): Unit = {
      currentMainStep match {
        case Some(step) =>
          stepsToReturn += step.elideWithFollowingSteps(steps)
        case None =>
          stepsToReturn ++= removeNonEndStructuralSimplifications(steps)
      }
    }

    for (currentStep <- steps) {
      provingContext.entryContext.typeStatementDefinitionsByType.find { case (_, definitions) =>
        definitions.mapCollect(_.deconstructionInference).map(_.id).contains(currentStep.inference.id)
      } match {
        case Some((typeSymbol, _)) =>
          groupSteps(removeUnnecessarySimplifications(currentUngroupedSteps.result(), typeSymbol))
          currentMainStep = Some(currentStep)
          currentUngroupedSteps.clear()
        case None =>
          currentUngroupedSteps += currentStep
      }
    }
    groupSteps(currentUngroupedSteps.result())
    stepsToReturn.result()
  }
}
