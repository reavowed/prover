package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.model.definitions.{BinaryRelationStatement, KnownStatement, TermDefinition, Wrapper}
import net.prover.model.expressions._
import net.prover.model.proof.StepProvingContext.KnownEquality
import net.prover.model.utils.ExpressionUtils
import net.prover.model.{Inference, Substitutions}

object PremiseFinder {

  def findDerivationsForStatements(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    premiseStatements.map(findDerivationForStatement).traverseOption.map(_.flatten)
  }

  def findDerivationForStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    stepProvingContext.cachedDerivations.getOrElseUpdate(
      targetStatement.serializedForHash,
      findDerivationForStatementUncached(targetStatement))
  }

  private def findDerivationForStatementUncached(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    stepProvingContext.provingContext.findRelation(targetStatement).map(findDirectDerivationForBinaryRelationStatement)
      .getOrElse(findDirectDerivationForStatement(targetStatement))
      .map(_.deduplicate)
  }

  private def findDerivationForStatementFromFacts(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    import stepProvingContext._
    import provingContext._
    def findDerivationWithFactInferences(targetStatement: Statement): Option[(Seq[DerivationStep], Seq[Inference])] = {
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

  private def findDirectDerivationForStatement(
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
        premiseSteps <- findDerivationsForStatements(premiseStatements)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ DerivationStep.fromAssertion(assertionStep)
    }
    def byRemovingTermDefinition = (for {
      termDefinition <- targetStatement.referencedDefinitions.ofType[TermDefinition].iterator
      inferenceExtraction <- provingContext.termDefinitionRemovals(termDefinition)
      substitutions <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality(inferenceExtraction.variableDefinitions))
      premiseStatements <- inferenceExtraction.premises.map(_.applySubstitutions(substitutions)).traverseOption
      premiseSteps <- findDerivationsForStatements(premiseStatements)
      derivationStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions)
    } yield premiseSteps :+ derivationStep).headOption
    fromPremises orElse fromFact orElse bySimplifyingTarget orElse byRemovingTermDefinition
  }

  private def findDirectDerivationForBinaryRelationStatement(
    binaryRelationStatement: BinaryRelationStatement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    import stepProvingContext._
    def withoutRewriting(binaryRelationStatement: BinaryRelationStatement): Option[Seq[DerivationStep]] = {
      def bySimplifyingTargetRelation = provingContext.conclusionRelationSimplificationInferences.getOrElse(binaryRelationStatement.relation, Nil).iterator.findFirst { conclusionRelationSimplificationInference =>
        for {
          (directTargets, binaryRelationTargets, derivationForInference) <- conclusionRelationSimplificationInference.getConclusionSimplification(binaryRelationStatement.baseStatement)
          derivationForDirectTargets <- directTargets.map(findDerivationForStatement).traverseOption.map(_.flatten)
          if !binaryRelationTargets.contains(binaryRelationStatement)
          derivationForBinaryRelationTargets <- binaryRelationTargets.map(findDirectDerivationForBinaryRelationStatement).traverseOption.map(_.flatten)
        } yield derivationForDirectTargets ++ derivationForBinaryRelationTargets ++ derivationForInference
      }
      findDirectDerivationForStatement(binaryRelationStatement.baseStatement) orElse bySimplifyingTargetRelation
    }
    def withoutRenaming(binaryRelationStatement: BinaryRelationStatement): Option[Seq[DerivationStep]] = {
      withoutRewriting(binaryRelationStatement) orElse {
        (for {
          inference <- provingContext.conclusionRelationRewriteInferences.getOrElse(binaryRelationStatement.relation, Nil)
          (rewrittenStatement, rewriteDerivation) <- inference.rewriteTarget(binaryRelationStatement.baseStatement)
          innerDerivation <- withoutRewriting(rewrittenStatement)
        } yield innerDerivation ++ rewriteDerivation).headOption
      }
    }
    withoutRenaming(binaryRelationStatement) orElse
    {
      (for {
        KnownEquality(source, result, equality, equalityDerivation) <- stepProvingContext.knownEqualities
        if result == binaryRelationStatement.right
        innerDerivation <- withoutRenaming(binaryRelationStatement.withNewRight(source))
        renameStep = DerivationStep.fromAssertion(equality.substitution.assertionStep(source, result, Wrapper[Term, Statement]((t, c) => binaryRelationStatement.relation(binaryRelationStatement.left, t)(c))))
      } yield innerDerivation ++ equalityDerivation :+ renameStep).headOption
    }
  }

  private def rewriteTarget(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Statement) = {
    stepProvingContext.knownValuesToProperties.foldLeft((premiseStatement, Seq.empty[DerivationStep])) { case ((currentStatement, currentDerivation), propertyValue) =>
      EqualityRewriter.getReverseReplacements(currentStatement, propertyValue.lhs, propertyValue.rhs, propertyValue.equality) match {
        case Some((result, derivationStep)) =>
          (result, currentDerivation ++ propertyValue.derivation :+ derivationStep)
        case None =>
          (currentStatement, currentDerivation)
      }
    }.swap
  }

  private def deconstruct(
    statement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(DerivationStep, Seq[Statement])] = {
    stepProvingContext.provingContext.statementDefinitionDeconstructions.mapFind { deconstructionInference =>
      for {
        substitutions <- deconstructionInference.conclusion.calculateSubstitutions(statement).flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
        step <- Step.Assertion.forInference(deconstructionInference, substitutions)
      } yield (DerivationStep.fromAssertion(step), step.premises.map(_.statement))
    }
  }

  private def splitTarget(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Statement]) = {
    def default = (Nil, Seq(targetStatement))
    if (ExpressionUtils.isTypeLikeStatement(targetStatement)) {
      default
    } else {
      deconstruct(targetStatement) match {
        case Some((deconstructionStep, innerTargets)) => splitTargets(innerTargets).mapLeft(deconstructionStep +: _)
        case None => default
      }
    }
  }

  private def splitTargets(
    targetStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Statement]) = {
    targetStatements.map(splitTarget).splitFlatten
  }

  private def findDerivationsOrTargets(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Step.Target]) = {
    val directly = findDerivationForStatement(premiseStatement).map(_ -> Nil)
    def byDeconstructing = for {
      (step, deconstructedStatements) <- deconstruct(premiseStatement)
      (innerSteps, innerTargets) = findDerivationsOrTargets(deconstructedStatements)
      if innerSteps.nonEmpty
    } yield (innerSteps :+ step, innerTargets)
    def asTarget = {
      val (rewriteSteps, rewrittenStatement) = rewriteTarget(premiseStatement)
      val (deconstructionSteps, deconstructedStatements) = splitTarget(rewrittenStatement)
      (rewriteSteps ++ deconstructionSteps, deconstructedStatements.map(Step.Target(_)))
    }
    directly orElse byDeconstructing getOrElse asTarget
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

  def findKnownStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible,
    knownStatements: Seq[KnownStatement])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(KnownStatement, Substitutions.Possible)] = {
    for {
      knownStatement <- knownStatements
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(knownStatement.statement, initialSubstitutions).toSeq
    } yield (knownStatement, finalSubstitutions)
  }

  def findDerivationForStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible,
    knownStatements: Seq[KnownStatement])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(KnownStatement, Substitutions.Possible)] = {
    import stepProvingContext._

    def directly = findKnownStatementBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions, knownStatements)

    def fromFact = for {
      (fact, substitutions) <- ProofHelper.findFactBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions)
    } yield (KnownStatement.fromSingleStep(fact), substitutions)

    def byDeconstructing = for {
      deconstructionInference <- provingContext.statementDefinitionDeconstructions
      initialDeconstructionSubstitutions <- deconstructionInference.conclusion.calculateSubstitutions(unsubstitutedPremiseStatement).flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
      deconstructedUnsubstitutedPremiseStatements <- deconstructionInference.premises.map(_.applySubstitutions(initialDeconstructionSubstitutions)).traverseOption
      (foundStatements, innerSubstitutions) <- findDerivationsForStatementsBySubstituting(deconstructedUnsubstitutedPremiseStatements, initialSubstitutions, knownStatements)
      deconstructionPremisesWithDeconstructedStatements <- deconstructionInference.premises.zipStrict(foundStatements)
      finalDeconstructionSubstitutions <- deconstructionPremisesWithDeconstructedStatements.foldLeft(Option(Substitutions.Possible.empty)) { case (substitutionsOption, (premise, knownStatement)) =>
        substitutionsOption.flatMap(premise.calculateSubstitutions(knownStatement.statement, _))
      }.flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
      deconstructionStep <- Step.Assertion.forInference(deconstructionInference, finalDeconstructionSubstitutions)
    } yield (KnownStatement.fromDerivation(foundStatements.flatMap(_.derivation) :+ DerivationStep.fromAssertion(deconstructionStep)), innerSubstitutions)

    unsubstitutedPremiseStatement.tryApplySubstitutions(initialSubstitutions) match {
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

  def findDerivationsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[KnownStatement], Substitutions.Possible)] = {
    findDerivationsForStatementsBySubstituting(unsubstitutedPremiseStatements, substitutions, stepProvingContext.knownStatementsFromPremises)
  }
}
