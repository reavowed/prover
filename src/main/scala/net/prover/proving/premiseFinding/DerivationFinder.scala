package net.prover.proving.premiseFinding

import net.prover.model.{Substitutions, _}
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.model.unwrapping.UnwrappedStatement
import net.prover.proving.derivation.{DerivedStatement, PremiseDerivation}

/**
  * Given a statement or a list of statements, attempts to automatically derive them from known statements (either
  * statements that have already been proved in the current proof, or facts that have been proved elsewhere).
  */
object DerivationFinder {
  def findDerivationForStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[PremiseDerivation] = {
    stepProvingContext.cachedDerivations.getOrElseUpdate(
      targetStatement.serializedForHash,
      findDerivationForStatementUncached(targetStatement))
  }

  private def findDerivationForStatementUncached(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[PremiseDerivation] = {
    UnwrappedStatement.getUnwrappedStatements(targetStatement).mapFind { unwrappedStatement =>
      findDerivationForUnwrappedStatement(unwrappedStatement.statement)(unwrappedStatement.unwrappers.enhanceStepProvingContext)
        .map(PremiseDerivation(unwrappedStatement.unwrappers, _))
    }
  }

  def findDerivationForUnwrappedStatements(
    targetStatement: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivedStatement]] = {
    targetStatement.map(findDerivationForUnwrappedStatement).traverseOption
  }

  def findDerivationForUnwrappedStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[DerivedStatement] = {
    stepProvingContext.provingContext.asBinaryRelationStatement(targetStatement).map(BinaryRelationDerivationFinder.findDirectDerivationForBinaryRelationStatement)
      .getOrElse(DirectDerivationFinder.findDirectDerivationForStatement(targetStatement))
  }

  def findDerivedStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible,
    derivedStatements: Seq[DerivedStatement])(
    implicit substitutionContext: SubstitutionContext
  ): Seq[(DerivedStatement, Substitutions.Possible)] = {
    for {
      derivedStatement <- derivedStatements
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(derivedStatement.statement, initialSubstitutions).toSeq
    } yield (derivedStatement, finalSubstitutions)
  }

  def findDerivationForStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible,
    derivedStatements: Seq[DerivedStatement])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(DerivedStatement, Substitutions.Possible)] = {
    def directly = findDerivedStatementBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions, derivedStatements)

    def fromFact = for {
      (fact, substitutions) <- ProofHelper.findFactBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions)
    } yield (fact.toDerivedStatement, substitutions)

    def byDeconstructing = for {
      deconstructionInference <- stepProvingContext.provingContext.statementDefinitionDeconstructions
      initialDeconstructionSubstitutions <- deconstructionInference.conclusion.calculateSubstitutions(unsubstitutedPremiseStatement).flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
      deconstructedUnsubstitutedPremiseStatements <- deconstructionInference.premises.map(_.applySubstitutions(initialDeconstructionSubstitutions)).traverseOption
      (foundStatements, innerSubstitutions) <- findDerivationsForStatementsBySubstituting(deconstructedUnsubstitutedPremiseStatements, initialSubstitutions, derivedStatements)
      deconstructionPremisesWithDeconstructedStatements <- deconstructionInference.premises.zipStrict(foundStatements)
      finalDeconstructionSubstitutions <- deconstructionPremisesWithDeconstructedStatements.foldLeft(Option(Substitutions.Possible.empty)) { case (substitutionsOption, (premise, derivedStatement)) =>
        substitutionsOption.flatMap(premise.calculateSubstitutions(derivedStatement.statement, _))
      }.flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
      deconstructionStep <- Step.AssertionStep.forInference(deconstructionInference, finalDeconstructionSubstitutions)
    } yield (DerivedStatement.Combined(foundStatements, deconstructionStep), innerSubstitutions)

    unsubstitutedPremiseStatement.tryApplySubstitutions(initialSubstitutions) match {
      case Some(substitutedPremiseStatement) =>
        for {
          premiseDerivation <- findDerivationForUnwrappedStatement(substitutedPremiseStatement).toSeq
        } yield (premiseDerivation.withKnownStatement(substitutedPremiseStatement), initialSubstitutions)
      case None =>
        directly ++ fromFact ++ byDeconstructing
    }
  }

  def findDerivationsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible,
    derivedStatements: Seq[DerivedStatement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[DerivedStatement], Substitutions.Possible)] = {
    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions.Possible, foundStatementsSoFar: Seq[DerivedStatement]): Option[(Seq[DerivedStatement], Substitutions.Possible)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          (for {
            (foundStatement, newSubstitutions) <- findDerivationForStatementBySubstituting(unsubstitutedPremise, currentSubstitutions, derivedStatements)
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
  ): Option[(Seq[DerivedStatement], Substitutions.Possible)] = {
    findDerivationsForStatementsBySubstituting(unsubstitutedPremiseStatements, substitutions, stepProvingContext.derivedStatementsFromPremises)
  }

  /**
    * Rewrites known property values in a statement to their values, e.g. replacing "domain(f)" with "A" given that
    * "A = domain(f)" is known. Returns the rewritten statement, along with a function that takes a derivation of the
    * rewritten statement to a derivation of the original one.
    */
  def rewriteWithKnownValues(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Statement, Option[DerivedStatement => DerivedStatement]) = {
    // Each rewrite proves the statement before it from the statement after it, so the outermost rewrite comes first
    val (rewrittenStatement, rewrites) = stepProvingContext.knownValuesToProperties.foldLeft((premiseStatement, Seq.empty[(DerivedStatement, Step.AssertionStep)])) { case ((currentStatement, rewritesSoFar), propertyValue) =>
      EqualityRewriter.getReverseReplacements(currentStatement, propertyValue.lhs, propertyValue.rhs, propertyValue.equality) match {
        case Some((result, derivationStep)) =>
          (result, rewritesSoFar :+ (propertyValue.derivation, derivationStep))
        case None =>
          (currentStatement, rewritesSoFar)
      }
    }
    def applyRewrites(rewrittenStatementDerivation: DerivedStatement): DerivedStatement = {
      rewrites.foldRight(rewrittenStatementDerivation) { case ((equalityDerivation, rewriteStep), innerDerivation) =>
        DerivedStatement.Substituted(innerDerivation, equalityDerivation, rewriteStep)
      }
    }
    (rewrittenStatement, Some(applyRewrites).filter(_ => rewrites.nonEmpty))
  }
}
