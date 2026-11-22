package net.prover.proving.premiseFinding

import net.prover.model.definitions.TermDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.model._
import net.prover.model.{Inference, Substitutions}
import net.prover.proving.derivation.DerivedStatement
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction, ExtractionApplier}

object DirectDerivationFinder {
  def findDirectDerivationForStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[DerivedStatement] = {
    import stepProvingContext._
    def fromPremises = knownStatementsFromPremisesBySerializedStatement.get(targetStatement.serializedForHash)

    def fromFact = findDerivationForStatementFromFact(targetStatement)

    def fromSimpleAssertion = (for {
      inferenceExtraction <- provingContext.simpleAssertionInferences
      substitutions <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement).flatMap(
        _.confirmTotality(inferenceExtraction.variableDefinitions))
      (derivationStep, premises) <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
      if premises.isEmpty
    } yield DerivedStatement.Asserted(derivationStep)).headOption

    def byRemovingTermDefinition = (for {
      termDefinition <- targetStatement.referencedDefinitions.ofType[TermDefinition].iterator
      inferenceExtraction <- provingContext.termDefinitionRemovals(termDefinition)
      substitutions <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement).flatMap(
        _.confirmTotality(inferenceExtraction.variableDefinitions))
      (derivationStep, premises) <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
      premiseDerivations <- DerivationFinder.findDerivationForUnwrappedStatements(premises)
    } yield DerivedStatement.Composed(premiseDerivations, derivationStep)).headOption

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseDerivations <- DerivationFinder.findDerivationForUnwrappedStatements(premiseStatements)
        assertionStep = Step.AssertionStep(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield DerivedStatement.Composed(premiseDerivations, assertionStep)
    }

    def byRewriting = for {
      (rewrittenTarget, applyRewritesOption) <- Some(DerivationFinder.rewriteWithKnownValues(targetStatement))
      applyRewrites <- applyRewritesOption
      rewrittenTargetDerivation <- findDirectDerivationForStatement(rewrittenTarget)
    } yield applyRewrites(rewrittenTargetDerivation)

    fromPremises orElse fromFact orElse fromSimpleAssertion orElse byRemovingTermDefinition orElse bySimplifyingTarget orElse byRewriting
  }

  private def findDerivationForStatementFromFact(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[DerivedStatement] = {
    import stepProvingContext.provingContext._

    def findDerivationWithFactInferences(targetStatement: Statement): Option[(Seq[AppliedExtractionStep], Option[Inference])] = {
      def directly = factsBySerializedStatement.get(targetStatement.serialized).map(fact =>
        (fact.extraction.extractionSteps, Some(fact.inference))
      )

      def bySimplifying = conclusionSimplificationInferences.iterator.findFirst { inference =>
        for {
          substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
          premiseStatements <- inference.substitutePremises(substitutions)
          (premiseDerivations, premiseFacts) <- premiseStatements.map(findDerivationWithFactInferences).traverseOption.map(_.split)
          singleFact = premiseFacts.traverseOption.flatMap(_.distinct.single)
          assertionStep = Step.AssertionStep(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
        } yield (premiseDerivations.reduce(_ ++ _) :+ assertionStep, singleFact)
      }

      directly orElse bySimplifying
    }

    findDerivationWithFactInferences(targetStatement) flatMap {
      case (derivation, Some(factInference)) =>
        val assertion = Step.AssertionStep(
          factInference.conclusion,
          factInference.summary,
          Nil,
          Substitutions.empty)
        Some(DerivedStatement.Asserted(AppliedInferenceExtraction(assertion, AppliedExtraction(derivation.distinctBy(_.statement), Nil))))
      case _ =>
        None
    }
  }
}
