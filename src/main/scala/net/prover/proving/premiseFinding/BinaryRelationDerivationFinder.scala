package net.prover.proving.premiseFinding

import net.prover.model._
import net.prover.model.definitions.Wrapper
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{KnownEquality, StepProvingContext}
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.derivation.DerivedStatement
import net.prover.proving.structure.statements.BinaryRelationStatement

object BinaryRelationDerivationFinder {
  def findDirectDerivationForBinaryRelationStatement(
    binaryRelationStatement: BinaryRelationStatement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[DerivedStatement] = {
    def withoutRewriting(binaryRelationStatement: BinaryRelationStatement): Option[DerivedStatement] = {
      def bySimplifyingTargetRelation = stepProvingContext.provingContext.conclusionRelationSimplificationInferences.getOrElse(binaryRelationStatement.relation, Nil).iterator.findFirst { conclusionRelationSimplificationInference =>
        for {
          (directTargets, binaryRelationTargets, deriveFromSimplifiedTargets) <- conclusionRelationSimplificationInference.getConclusionSimplification(binaryRelationStatement.baseStatement)
          derivationsForDirectTargets <- directTargets.map(DerivationFinder.findDerivationForUnwrappedStatement).traverseOption
          if !binaryRelationTargets.contains(binaryRelationStatement)
          derivationsForBinaryRelationTargets <- binaryRelationTargets.map(findDirectDerivationForBinaryRelationStatement).traverseOption
        } yield deriveFromSimplifiedTargets(derivationsForDirectTargets, derivationsForBinaryRelationTargets)
      }

      DirectDerivationFinder.findDirectDerivationForStatement(binaryRelationStatement.baseStatement) orElse bySimplifyingTargetRelation
    }

    def withoutRenaming(binaryRelationStatement: BinaryRelationStatement): Option[DerivedStatement] = {
      withoutRewriting(binaryRelationStatement) orElse {
        (for {
          inference <- stepProvingContext.provingContext.conclusionRelationRewriteInferences.getOrElse(binaryRelationStatement.relation, Nil)
          (rewrittenStatement, applyRewrite) <- inference.rewriteTarget(binaryRelationStatement.baseStatement)
          innerDerivation <- withoutRewriting(rewrittenStatement)
        } yield applyRewrite(innerDerivation)).headOption
      }
    }

    def byRenaming: Option[DerivedStatement] = {
      def directly = (for {
        KnownEquality(source, result, equality, equalityDerivation) <- stepProvingContext.knownEqualities
        if result == binaryRelationStatement.right
        innerDerivation <- withoutRenaming(binaryRelationStatement.withNewRight(source))
        renameStep = equality.substitution.assertionStep(source, result, Wrapper[Term, Statement]((t, c) => binaryRelationStatement.relation(binaryRelationStatement.left, t)(c)))
      } yield DerivedStatement.Substituted(innerDerivation, equalityDerivation, renameStep)).headOption

      def transitively = (for {
        secondEquality <- stepProvingContext.knownEqualities
        equality = secondEquality.equality
        if secondEquality.rhs == binaryRelationStatement.right && ExpressionUtils.isSimpleTermVariableOrCombinationOfTermConstants(secondEquality.lhs)
        firstEquality <- stepProvingContext.knownEqualities
        if firstEquality.rhs == secondEquality.lhs
        innerDerivation <- withoutRenaming(binaryRelationStatement.withNewRight(firstEquality.lhs))
        transitivityStep = equality.transitivity.assertionStep(firstEquality.lhs, firstEquality.rhs, secondEquality.rhs)
        renameStep = equality.substitution.assertionStep(firstEquality.lhs, secondEquality.rhs, Wrapper[Term, Statement]((t, c) => binaryRelationStatement.relation(binaryRelationStatement.left, t)(c)))
        equalityDerivation = DerivedStatement.Transitive(firstEquality.derivation, secondEquality.derivation, transitivityStep)
      } yield DerivedStatement.Substituted(innerDerivation, equalityDerivation, renameStep)).headOption

      directly orElse transitively
    }

    withoutRenaming(binaryRelationStatement) orElse byRenaming
  }
}
