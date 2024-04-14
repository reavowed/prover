package net.prover.proving.premiseFinding

import net.prover.model.definitions.TermDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.model.{Inference, Substitutions}
import net.prover.proving.extraction.ExtractionApplier

object DirectDerivationFinder {
  def findDirectDerivationForStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[Step.AssertionOrExtraction]] = {
    import stepProvingContext._
    def fromPremises = knownStatementsFromPremisesBySerializedStatement.get(targetStatement.serializedForHash).map(_.derivation)

    def fromFact = findDerivationForStatementFromFact(targetStatement).map(Seq(_))

    def byRemovingTermDefinition = (for {
      termDefinition <- targetStatement.referencedDefinitions.ofType[TermDefinition].iterator
      inferenceExtraction <- provingContext.termDefinitionRemovals(termDefinition)
      substitutions <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inferenceExtraction.variableDefinitions))
      premiseStatements <- inferenceExtraction.premises.map(_.applySubstitutions(substitutions)).traverseOption
      premiseSteps <- DerivationFinder.findDerivationForUnwrappedStatements(premiseStatements)
      derivationStep <- ExtractionApplier.getInferenceExtractionStepWithoutPremises(inferenceExtraction, substitutions)
    } yield premiseSteps :+ derivationStep).headOption

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- DerivationFinder.findDerivationForUnwrappedStatements(premiseStatements)
        assertionStep = Step.AssertionStep(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ assertionStep
    }

    def byRewriting = {
      val (rewriteSteps, rewrittenTarget) = DerivationFinder.rewriteWithKnownValues(targetStatement)
      if (rewriteSteps.nonEmpty) findDirectDerivationForStatement(rewrittenTarget).map(_ ++ rewriteSteps) else None
    }

    fromPremises orElse fromFact orElse byRemovingTermDefinition orElse bySimplifyingTarget orElse byRewriting
  }

  private def findDerivationForStatementFromFact(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Step.AssertionOrExtraction] = {
    import stepProvingContext.provingContext._

    def findDerivationWithFactInferences(targetStatement: Statement): Option[(Seq[Step.AssertionOrExtraction], Option[Inference])] = {
      def directly = factsBySerializedStatement.get(targetStatement.serialized).map(fact => (fact.derivation, Some(fact.inference)))

      def bySimplifying = conclusionSimplificationInferences.iterator.findFirst { inference =>
        for {
          substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
          premiseStatements <- inference.substitutePremises(substitutions)
          (premiseDerivations, premiseFacts) <- premiseStatements.map(findDerivationWithFactInferences).traverseOption.map(_.split)
          singleFact = premiseFacts.traverseOption.flatMap(_.distinct.single)
          assertionStep = Step.AssertionStep(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
        } yield (premiseDerivations.flatten :+ assertionStep, singleFact)
      }

      directly orElse bySimplifying
    }

    findDerivationWithFactInferences(targetStatement) flatMap {
      case (derivationSteps, Some(factInference)) =>
        val assertion = Step.AssertionStep(
          factInference.conclusion,
          factInference.summary,
          Nil,
          Substitutions.empty)
        Some(assertion.addExtractionSteps(derivationSteps.distinctBy(_.statement)))
      case _ =>
        None
    }
  }
}
