package net.prover.proving.premiseFinding

import net.prover.model.Inference
import net.prover.model.definitions.TermDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.proving.extraction.ExtractionHelper

object DirectDerivationFinder {
  def findDirectDerivationForStatement(
    targetStatement: Statement)(
    implicit stepContext: StepContext
  ): Option[Seq[Step.InferenceApplicationWithoutPremises]] = {
    import stepContext._
    def fromPremises = knownStatementsFromPremisesBySerializedStatement.get(targetStatement.serialized).map(_.derivation)

    def fromFact = findDerivationForStatementFromFacts(targetStatement)

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- DerivationFinder.findDerivationForUnwrappedStatements(premiseStatements)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ assertionStep
    }

    def byRemovingTermDefinition = (for {
      termDefinition <- targetStatement.referencedDefinitions.ofType[TermDefinition].iterator
      inferenceExtraction <- provingContext.termDefinitionRemovals(termDefinition)
      substitutions <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inferenceExtraction.variableDefinitions))
      premiseStatements <- inferenceExtraction.premises.map(_.applySubstitutions(substitutions)).traverseOption
      premiseSteps <- DerivationFinder.findDerivationForUnwrappedStatements(premiseStatements)
      derivationStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions)
    } yield premiseSteps :+ derivationStep).headOption

    fromPremises orElse fromFact orElse bySimplifyingTarget orElse byRemovingTermDefinition
  }

  private def findDerivationForStatementFromFacts(
    targetStatement: Statement)(
    implicit stepContext: StepContext
  ): Option[Seq[Step.InferenceApplicationWithoutPremises]] = {
    import stepContext.provingContext._
    def findDerivationWithFactInferences(targetStatement: Statement): Option[(Seq[Step.InferenceApplicationWithoutPremises], Seq[Inference])] = {
      def fromPremise = stepContext.allPremises.find(_.statement == targetStatement).map(_ => (Nil, Nil))

      def directly = factsBySerializedStatement.get(targetStatement.serialized).map(derivationStep => (Seq(derivationStep), Seq(derivationStep.inference)))

      def bySimplifying = conclusionSimplificationInferences.iterator.findFirst { inference =>
        for {
          substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
          premiseStatements <- inference.substitutePremises(substitutions)
          (premiseDerivations, premiseFacts) <- premiseStatements.map(findDerivationWithFactInferences).traverseOption.map(_.splitFlatten)
          assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
        } yield (premiseDerivations :+ assertionStep, premiseFacts)
      }

      fromPremise orElse directly orElse bySimplifying
    }

    findDerivationWithFactInferences(targetStatement) map { case (derivationSteps, factInferences) =>
      factInferences.distinct match {
        case Seq(_) if derivationSteps.length > 1 =>
          Seq(Step.InferenceExtraction(derivationSteps))
        case _ =>
          derivationSteps
      }
    }
  }
}
