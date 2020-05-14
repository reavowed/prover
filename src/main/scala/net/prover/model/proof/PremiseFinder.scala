package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.model.Substitutions
import net.prover.model.definitions.{KnownStatement, TermDefinition}
import net.prover.model.expressions._

object PremiseFinder {

  def findDerivationForStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    stepProvingContext.cachedDerivations.getOrElseUpdate(
      targetStatement.serializedForHash,
      findDerivationForStatementUncached(targetStatement))
  }

  def findDerivationForStatementUncached(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    import stepProvingContext._

    def fromPremises = knownStatementsFromPremisesBySerializedStatement.get(targetStatement.serialized).map(_.derivation)
    def fromFact = provingContext.factsBySerializedStatement.get(targetStatement.serialized).map(Seq(_))

    def byRemovingTermDefinition = (for {
      termDefinition <- targetStatement.referencedDefinitions.ofType[TermDefinition].iterator
      extractionOption <- provingContext.termDefinitionRemovals(termDefinition)
      substitutions <- extractionOption.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
      premiseStatements <- extractionOption.premises.map(_.applySubstitutions(substitutions)).traverseOption
      premiseSteps <- findDerivationsForStatements(premiseStatements)
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(termDefinition.definitionInference, substitutions, extractionOption)
    } yield premiseSteps :+ derivationStep).headOption

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- findDerivationsForStatements(premiseStatements)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ DerivationStep.fromAssertion(assertionStep)
    }
    def bySimplifyingTargetRelation = provingContext.conclusionRelationSimplificationInferences.iterator.findFirst { conclusionRelationSimplificationInference =>
      for {
        (simplifiedTargets, stepsForInference) <- conclusionRelationSimplificationInference.getConclusionSimplification(targetStatement)
        stepsForSimplifiedTarget <- findDerivationsForStatements(simplifiedTargets)
      } yield stepsForSimplifiedTarget ++ stepsForInference
    }

    (fromPremises orElse fromFact orElse byRemovingTermDefinition orElse bySimplifyingTarget orElse bySimplifyingTargetRelation).map(_.deduplicate)
  }

  def findDerivationsForStatements(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    premiseStatements.map(findDerivationForStatement).traverseOption.map(_.flatten)
  }

  private def getTarget(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Step.Target) = {
    stepProvingContext.renamedTerms.foldLeft((Seq.empty[DerivationStep], premiseStatement)) { case ((currentDerivation, currentStatement), (lhs, rhs, renameDerivation, equality)) =>
      EqualityRewriter.getReverseReplacements(currentStatement, lhs, rhs, equality) match {
        case Some((result, derivationStep)) =>
          (currentDerivation ++ renameDerivation :+ derivationStep, result)
        case None =>
          (currentDerivation, currentStatement)
      }
    }.mapRight(Step.Target(_))
  }

  private def findDerivationsOrTargets(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Step.Target]) = {
    val directly = PremiseFinder.findDerivationForStatement(premiseStatement).map(_ -> Nil)
    def byDeconstructing = (for {
      deconstructionInference <- stepProvingContext.provingContext.statementDefinitionDeconstructions
      substitutions <- deconstructionInference.conclusion.calculateSubstitutions(premiseStatement).flatMap(_.confirmTotality).toSeq
      step <- Step.Assertion.forInference(deconstructionInference, substitutions).toSeq
      (innerSteps, innerTargets) = findDerivationsOrTargets(step.premises.map(_.statement))
    } yield (innerSteps :+ DerivationStep.fromAssertion(step), innerTargets)).headOption

    directly orElse byDeconstructing getOrElse getTarget(premiseStatement).mapRight(Seq(_))
  }

  def findDerivationsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Step.Target]) = {
    val (premiseSteps, targets) = premiseStatements.foldLeft((Seq.empty[DerivationStep], Seq.empty[Step.Target])) { case ((premiseStepsSoFar, targetStepsSoFar), premiseStatement) =>
      val (stepsForThisPremise, targetsForThisPremise) = findDerivationsOrTargets(premiseStatement)
      (premiseStepsSoFar ++ stepsForThisPremise, targetStepsSoFar ++ targetsForThisPremise)
    }
    (premiseSteps.deduplicate, targets)
  }

  def findDerivationForStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible,
    knownStatements: Seq[KnownStatement])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(KnownStatement, Substitutions.Possible)] = {
    import stepProvingContext._

    def directly = for {
      knownStatement <- knownStatements
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(knownStatement.statement, initialSubstitutions).toSeq
    } yield (knownStatement, finalSubstitutions)

    def fromFact = for {
      (fact, substitutions) <- ProofHelper.findFactBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions)
    } yield (KnownStatement.fromSingleStep(fact), substitutions)

    def byDeconstructing = for {
      deconstructionInference <- provingContext.statementDefinitionDeconstructions
      initialDeconstructionSubstitutions <- deconstructionInference.conclusion.calculateSubstitutions(unsubstitutedPremiseStatement).flatMap(_.confirmTotality)
      deconstructedUnsubstitutedPremiseStatements <- deconstructionInference.premises.map(_.applySubstitutions(initialDeconstructionSubstitutions)).traverseOption
      (foundStatements, innerSubstitutions) <- findDerivationsForStatementsBySubstituting(deconstructedUnsubstitutedPremiseStatements, initialSubstitutions, knownStatements)
      deconstructionPremisesWithDeconstructedStatements <- deconstructionInference.premises.zipStrict(foundStatements)
      finalDeconstructionSubstitutions <- deconstructionPremisesWithDeconstructedStatements.foldLeft(Option(Substitutions.Possible.empty)) { case (substitutionsOption, (premise, knownStatement)) =>
        substitutionsOption.flatMap(premise.calculateSubstitutions(knownStatement.statement, _))
      }.flatMap(_.confirmTotality)
      deconstructionStep <- Step.Assertion.forInference(deconstructionInference, finalDeconstructionSubstitutions)
    } yield (KnownStatement.fromDerivation(foundStatements.flatMap(_.derivation) :+ DerivationStep.fromAssertion(deconstructionStep)), innerSubstitutions)

    unsubstitutedPremiseStatement.applySubstitutions(initialSubstitutions.stripApplications()) match {
      case Some(substitutedPremiseStatement) =>
        for {
          premiseDerivation <- findDerivationForStatement(substitutedPremiseStatement).toSeq
        } yield (KnownStatement(substitutedPremiseStatement, premiseDerivation), initialSubstitutions)
      case None =>
        directly ++ fromFact ++ byDeconstructing
    }
  }

  def findDerivationsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible,
    knownStatements: Seq[KnownStatement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[KnownStatement], Substitutions.Possible)] = {
    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions.Possible, foundStatementsSoFar: Seq[KnownStatement]): Option[(Seq[KnownStatement], Substitutions.Possible)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          (for {
            (foundStatement, newSubstitutions) <- findDerivationForStatementBySubstituting(unsubstitutedPremise, currentSubstitutions, knownStatements)
            result <- helper(otherUnsubstitutedPremises, newSubstitutions, foundStatementsSoFar :+ foundStatement)
          } yield result).headOption
        case Nil =>
          Some((foundStatementsSoFar, currentSubstitutions))
      }
    }
    helper(unsubstitutedPremiseStatements, substitutions, Nil)
  }

  def findDerivationForStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(KnownStatement, Substitutions.Possible)] = {
    findDerivationForStatementBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions, stepProvingContext.knownStatementsFromPremises)
  }

  def findDerivationsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[KnownStatement], Substitutions.Possible)] = {
    findDerivationsForStatementsBySubstituting(unsubstitutedPremiseStatements, substitutions, stepProvingContext.knownStatementsFromPremises)
  }
}
