package net.prover.proving.premiseFinding

import net.prover.model.{Inference, Substitutions}
import net.prover.model.definitions.TermDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.proving.extraction.ExtractionApplier

object DirectDerivationFinder {
  def findDirectDerivationForStatement(
    targetStatement: Statement)(
    implicit stepContext: StepContext
  ): Option[Seq[Step.InferenceApplicationWithoutPremises]] = {
    import stepContext._
    def fromPremises = knownStatementsFromPremisesBySerializedStatement.get(targetStatement.serialized).map(_.derivation)

    def fromFact = findDerivationForStatementFromFact(targetStatement).map(Seq(_))

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
      derivationStep <- ExtractionApplier.getInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
    } yield premiseSteps :+ derivationStep).headOption

    fromPremises orElse fromFact orElse bySimplifyingTarget orElse byRemovingTermDefinition
  }

  private def findDerivationForStatementFromFact(
    targetStatement: Statement)(
    implicit stepContext: StepContext
  ): Option[Step.AssertionOrExtraction] = {
    import stepContext.provingContext._

    def findDerivationWithFactInferences(targetStatement: Statement): Option[(Seq[Step.AssertionOrExtraction], Option[Inference])] = {
      def directly = factsBySerializedStatement.get(targetStatement.serialized).map(fact => (fact.derivation, Some(fact.inference)))

      def bySimplifying = conclusionSimplificationInferences.iterator.findFirst { inference =>
        for {
          substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inference.variableDefinitions))
          premiseStatements <- inference.substitutePremises(substitutions)
          (premiseDerivations, premiseFacts) <- premiseStatements.map(findDerivationWithFactInferences).traverseOption.map(_.split)
          singleFact = premiseFacts.traverseOption.flatMap(_.distinct.single)
          assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
        } yield (premiseDerivations.flatten :+ assertionStep, singleFact)
      }

      directly orElse bySimplifying
    }

    findDerivationWithFactInferences(targetStatement) flatMap {
      case (derivationSteps, Some(factInference)) =>
        val assertion = Step.Assertion(
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
