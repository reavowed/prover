package net.prover.proving.premiseFinding

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.definitions.TermDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof._

object DirectDerivationFinder {
  def findDirectDerivationForStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    import stepProvingContext._
    def fromPremises = knownStatementsFromPremisesBySerializedStatement.get(targetStatement.serialized).map(_.derivation)

    def fromFact = findDerivationForStatementFromFacts(targetStatement)

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- DerivationFinder.findDerivationsForStatements(premiseStatements)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ DerivationStep.fromAssertion(assertionStep)
    }

    def byRemovingTermDefinition = (for {
      termDefinition <- targetStatement.referencedDefinitions.ofType[TermDefinition].iterator
      inferenceExtraction <- provingContext.termDefinitionRemovals(termDefinition)
      substitutions <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inferenceExtraction.variableDefinitions))
      premiseStatements <- inferenceExtraction.premises.map(_.applySubstitutions(substitutions)).traverseOption
      premiseSteps <- DerivationFinder.findDerivationsForStatements(premiseStatements)
      derivationStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions)
    } yield premiseSteps :+ derivationStep).headOption

    fromPremises orElse fromFact orElse bySimplifyingTarget orElse byRemovingTermDefinition
  }

  private def findDerivationForStatementFromFacts(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    import stepProvingContext._
    import provingContext._
    def findDerivationWithFactInferences(targetStatement: Statement): Option[(Seq[DerivationStepWithSingleInference], Seq[Inference])] = {
      def directly = factsBySerializedStatement.get(targetStatement.serialized).map(derivationStep => (Seq(derivationStep), Seq(derivationStep.inference)))

      def bySimplifying = conclusionSimplificationInferences.iterator.findFirst { inference =>
        for {
          substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
          premiseStatements <- inference.substitutePremises(substitutions)
          (premiseDerivations, premiseFacts) <- premiseStatements.map(findDerivationWithFactInferences).traverseOption.map(_.splitFlatten)
          assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
        } yield (premiseDerivations :+ DerivationStep.fromAssertion(assertionStep), premiseFacts)
      }

      directly orElse bySimplifying
    }

    findDerivationWithFactInferences(targetStatement) map { case (derivationSteps, factInferences) =>
      factInferences.distinct.single match {
        case Some(fact) if derivationSteps.length > 1 =>
          Seq(derivationSteps.elideWithInference(fact))
        case _ =>
          derivationSteps
      }
    }
  }
}
