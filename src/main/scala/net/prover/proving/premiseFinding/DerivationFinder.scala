package net.prover.proving.premiseFinding

import net.prover.model.Substitutions
import net.prover.model.definitions.KnownStatement
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.model.unwrapping.UnwrappedStatement

/**
  * Given a statement or a list of statements, attempts to automatically derive them from known statements (either
  * statements that have already been proved in the current proof, or facts that have been proved elsewhere).
  */
object DerivationFinder {
  def findDerivationsForStatements(
    premiseStatements: Seq[Statement])(
    implicit stepContext: StepContext
  ): Option[Seq[Step.PremiseDerivation]] = {
    premiseStatements.map(findDerivationForStatement).traverseOption.map(_.flatten)
  }

  def findDerivationForStatement(
    targetStatement: Statement)(
    implicit stepContext: StepContext
  ): Option[Seq[Step.PremiseDerivation]] = {
    stepContext.cachedDerivations.getOrElseUpdate(
      targetStatement.serializedForHash,
      findDerivationForStatementUncached(targetStatement))
  }

  private def findDerivationForStatementUncached(
    targetStatement: Statement)(
    implicit stepContext: StepContext
  ): Option[Seq[Step.PremiseDerivation]] = {
    UnwrappedStatement.getUnwrappedStatements(targetStatement).mapFind { unwrappedStatement =>
      findDerivationForUnwrappedStatement(unwrappedStatement.statement)(unwrappedStatement.unwrappers.enhanceStepContext)
        .map { derivation =>
          if (unwrappedStatement.unwrappers.nonEmpty) {
            Seq(Step.WrappedPremiseDerivation(unwrappedStatement.unwrappers, derivation))
          } else {
            derivation
          }
        }
    }
  }

  def findDerivationForUnwrappedStatements(
    targetStatement: Seq[Statement])(
    implicit stepContext: StepContext
  ): Option[Seq[Step.InferenceApplicationWithoutPremises]] = {
    targetStatement.map(findDerivationForUnwrappedStatement).traverseOption.map(_.flatten)
  }

  def findDerivationForUnwrappedStatement(
    targetStatement: Statement)(
    implicit stepContext: StepContext
  ): Option[Seq[Step.InferenceApplicationWithoutPremises]] = {
    stepContext.provingContext.findRelation(targetStatement).map(BinaryRelationDerivationFinder.findDirectDerivationForBinaryRelationStatement)
      .getOrElse(DirectDerivationFinder.findDirectDerivationForStatement(targetStatement))
      .map(_.distinctBy(_.statement))
  }

  def findKnownStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible,
    knownStatements: Seq[KnownStatement])(
    implicit substitutionContext: SubstitutionContext
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
    implicit stepContext: StepContext
  ): Seq[(KnownStatement, Substitutions.Possible)] = {
    def directly = findKnownStatementBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions, knownStatements)

    def fromFact = for {
      (fact, substitutions) <- ProofHelper.findFactBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions)
    } yield (KnownStatement.fromSingleStep(fact), substitutions)

    def byDeconstructing = for {
      deconstructionInference <- stepContext.provingContext.statementDefinitionDeconstructions
      initialDeconstructionSubstitutions <- deconstructionInference.conclusion.calculateSubstitutions(unsubstitutedPremiseStatement).flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
      deconstructedUnsubstitutedPremiseStatements <- deconstructionInference.premises.map(_.applySubstitutions(initialDeconstructionSubstitutions)).traverseOption
      (foundStatements, innerSubstitutions) <- findDerivationsForStatementsBySubstituting(deconstructedUnsubstitutedPremiseStatements, initialSubstitutions, knownStatements)
      deconstructionPremisesWithDeconstructedStatements <- deconstructionInference.premises.zipStrict(foundStatements)
      finalDeconstructionSubstitutions <- deconstructionPremisesWithDeconstructedStatements.foldLeft(Option(Substitutions.Possible.empty)) { case (substitutionsOption, (premise, knownStatement)) =>
        substitutionsOption.flatMap(premise.calculateSubstitutions(knownStatement.statement, _))
      }.flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
      deconstructionStep <- Step.Assertion.forInference(deconstructionInference, finalDeconstructionSubstitutions)
    } yield (KnownStatement.fromDerivation(foundStatements.flatMap(_.derivation) :+ deconstructionStep), innerSubstitutions)

    unsubstitutedPremiseStatement.tryApplySubstitutions(initialSubstitutions) match {
      case Some(substitutedPremiseStatement) =>
        for {
          premiseDerivation <- findDerivationForUnwrappedStatement(substitutedPremiseStatement).toSeq
        } yield (KnownStatement(substitutedPremiseStatement, premiseDerivation), initialSubstitutions)
      case None =>
        directly ++ fromFact ++ byDeconstructing
    }
  }

  def findDerivationsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible,
    knownStatements: Seq[KnownStatement])(
    implicit stepContext: StepContext
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
    implicit stepContext: StepContext
  ): Option[(Seq[KnownStatement], Substitutions.Possible)] = {
    findDerivationsForStatementsBySubstituting(unsubstitutedPremiseStatements, substitutions, stepContext.knownStatementsFromPremises)
  }
}
