package net.prover.proving.premiseFinding

import net.prover.model._
import net.prover.model.definitions.{BinaryRelationStatement, Wrapper}
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{KnownEquality, StepProvingContext}
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.derivation.SimpleDerivation

object BinaryRelationDerivationFinder {
  def findDirectDerivationForBinaryRelationStatement(
    binaryRelationStatement: BinaryRelationStatement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[SimpleDerivation] = {
    def withoutRewriting(binaryRelationStatement: BinaryRelationStatement): Option[SimpleDerivation] = {
      def bySimplifyingTargetRelation = stepProvingContext.provingContext.conclusionRelationSimplificationInferences.getOrElse(binaryRelationStatement.relation, Nil).iterator.findFirst { conclusionRelationSimplificationInference =>
        for {
          (directTargets, binaryRelationTargets, conclusionSimplifcationDerivation) <- conclusionRelationSimplificationInference.getConclusionSimplification(binaryRelationStatement.baseStatement)
          derivationForDirectTargets <- directTargets.map(DerivationFinder.findDerivationForUnwrappedStatement).traverseOption.map(_.join)
          if !binaryRelationTargets.contains(binaryRelationStatement)
          derivationForBinaryRelationTargets <- binaryRelationTargets.map(findDirectDerivationForBinaryRelationStatement).traverseOption.map(_.join)
        } yield derivationForDirectTargets ++ derivationForBinaryRelationTargets ++ conclusionSimplifcationDerivation
      }

      DirectDerivationFinder.findDirectDerivationForStatement(binaryRelationStatement.baseStatement) orElse bySimplifyingTargetRelation
    }

    def withoutRenaming(binaryRelationStatement: BinaryRelationStatement): Option[SimpleDerivation] = {
      withoutRewriting(binaryRelationStatement) orElse {
        (for {
          inference <- stepProvingContext.provingContext.conclusionRelationRewriteInferences.getOrElse(binaryRelationStatement.relation, Nil)
          (rewrittenStatement, rewriteDerivation) <- inference.rewriteTarget(binaryRelationStatement.baseStatement)
          innerDerivation <- withoutRewriting(rewrittenStatement)
        } yield innerDerivation ++ rewriteDerivation).headOption
      }
    }

    def byRenaming: Option[SimpleDerivation] = {
      def directly = (for {
        KnownEquality(source, result, equality, equalityDerivation) <- stepProvingContext.knownEqualities
        if result == binaryRelationStatement.right
        innerDerivation <- withoutRenaming(binaryRelationStatement.withNewRight(source))
        renameStep = equality.substitution.assertionStep(source, result, Wrapper[Term, Statement]((t, c) => binaryRelationStatement.relation(binaryRelationStatement.left, t)(c)))
      } yield innerDerivation ++ equalityDerivation :+ renameStep).headOption

      def transitively = (for {
        secondEquality <- stepProvingContext.knownEqualities
        equality = secondEquality.equality
        if secondEquality.rhs == binaryRelationStatement.right && ExpressionUtils.isSimpleTermVariableOrCombinationOfTermConstants(secondEquality.lhs)
        firstEquality <- stepProvingContext.knownEqualities
        if firstEquality.rhs == secondEquality.lhs
        innerDerivation <- withoutRenaming(binaryRelationStatement.withNewRight(firstEquality.lhs))
        transitivityStep = equality.transitivity.assertionStep(firstEquality.lhs, firstEquality.rhs, secondEquality.rhs)
        renameStep = equality.substitution.assertionStep(firstEquality.lhs, secondEquality.rhs, Wrapper[Term, Statement]((t, c) => binaryRelationStatement.relation(binaryRelationStatement.left, t)(c)))
      } yield innerDerivation ++ firstEquality.derivation ++ secondEquality.derivation :+ transitivityStep :+ renameStep).headOption

      directly orElse transitively
    }

    withoutRenaming(binaryRelationStatement) orElse byRenaming
  }
}
