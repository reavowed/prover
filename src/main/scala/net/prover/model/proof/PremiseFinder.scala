package net.prover.model.proof

import net.prover.model.{EntryContext, Inference, Substitutions}
import net.prover.model.expressions.Statement

import scala.util.Try

object PremiseFinder {

  def findPremiseSteps(
    premiseStatement: Statement,
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Option[Seq[Step]] = {
    val simplificationInferences = entryContext.availableEntries.ofType[Inference].filter {
      case inference @ Inference(_, premises, conclusion)
        if premises.nonEmpty &&
          premises.forall(_.complexity < conclusion.complexity) &&
          conclusion.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
          conclusion.requiredSubstitutions.predicates.isEmpty && conclusion.requiredSubstitutions.functions.isEmpty &&
          premises.forall(_.referencedDefinitions.subsetOf(conclusion.referencedDefinitions))
      =>
        true
      case _ =>
        false
    }

    def fromGivenPremises = premiseContext.premisesAndSimplifications
      .flatMap(x => x._2.reverse :+ x._1)
      .map(_.statement)
      .find(_ == premiseStatement)
      .map(_ => Nil)
    def fromFact = ProofHelper.findFact(premiseStatement, stepContext, entryContext).map(Seq(_))
    def bySimplifying = simplificationInferences.iterator.findFirst { inference =>
      (for {
        substitutions <- inference.conclusion.calculateSubstitutions(premiseStatement, Substitutions.empty, 0, stepContext.externalDepth)
        premiseStatements <- Try(inference.substitutePremises(substitutions, stepContext)).toOption
        premiseSteps <- findPremiseSteps(premiseStatements, entryContext, premiseContext, stepContext)
        assertionStep = Step.Assertion(premiseStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ assertionStep).headOption
    }
    fromGivenPremises orElse fromFact orElse bySimplifying
  }

  def findPremiseSteps(
    premiseStatements: Seq[Statement],
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Option[Seq[Step]] = {
    premiseStatements.map(findPremiseSteps(_, entryContext, premiseContext, stepContext)).traverseOption.map(_.flatten)
  }
}
