package net.prover.controllers

import net.prover.model.{ExpressionParsingContext, Parser, ProvingContext}
import net.prover.model.definitions.{BinaryConnective, BinaryJoiner, BinaryRelation, Reversal, Transitivity, Wrapper}
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.{Step, SubstitutionContext}

import scala.reflect.ClassTag

sealed trait ChainingMethods[T <: Expression] {
  def getRelation(statement: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(BinaryJoiner[T], T, T)]
  def getTransitivityStep(
    source: T,
    intermediate: T,
    target: T,
    firstJoiner: BinaryJoiner[T],
    secondJoiner: BinaryJoiner[T])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[(BinaryJoiner[T], Step)] = {
    for {
      transitivity <- provingContext.transitivities.ofType[Transitivity[T]].find(_.statement == firstJoiner)
      if firstJoiner == secondJoiner
    } yield firstJoiner -> transitivity.assertionStep(source, intermediate, target)
  }
  def parser(implicit expressionParsingContext: ExpressionParsingContext): Parser[T]
}
object ChainingMethods {
  abstract class ChainingMethodsAux[TExpression <: Expression, TStatement <: BinaryJoiner[TExpression] : ClassTag] extends ChainingMethods[TExpression] {
    override def getRelation(statement: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(BinaryJoiner[TExpression], TExpression, TExpression)] = {
      provingContext.definedBinaryStatements.ofType[TStatement].mapFind { relation =>
        for {
          (lhs, rhs) <- relation.unapply(statement)
        } yield (relation, lhs, rhs)
      }
    }
  }

  implicit object ForStatement2 extends ChainingMethodsAux[Statement, BinaryConnective] with ChainingMethods[Statement] {
    override def parser(implicit expressionParsingContext: ExpressionParsingContext): Parser[Statement] = Statement.parser
  }
  implicit object ForTerm extends ChainingMethodsAux[Term, BinaryRelation] with ChainingMethods[Term] {
    override def getTransitivityStep(
      source: Term,
      intermediate: Term,
      target: Term,
      firstRelation: BinaryJoiner[Term],
      secondRelation: BinaryJoiner[Term])(
      implicit provingContext: ProvingContext,
      substitutionContext: SubstitutionContext
    ): Option[(BinaryJoiner[Term], Step)] = {
      def bySubstitutionFromFirst = for {
        substitution <- provingContext.substitutions.find(_.relation == firstRelation)
        reversal <- provingContext.reversals.ofType[Reversal[Term]].find(_.relation == firstRelation)
      } yield {
        secondRelation -> Step.Elided.forInference(substitution.inference)(Seq(
          reversal.assertionStep(intermediate, source),
          substitution.assertionStep(intermediate, source, new Wrapper(secondRelation(_, target)(_)))))
      }
      def bySubstitutionFromSecond = for {
        substitution <- provingContext.substitutions.find(_.relation == secondRelation)
      } yield firstRelation -> substitution.assertionStep(intermediate, target, new Wrapper(firstRelation(source, _)(_)))
      super.getTransitivityStep(source, intermediate, target, firstRelation, secondRelation) orElse bySubstitutionFromFirst orElse bySubstitutionFromSecond
    }
    override def parser(implicit expressionParsingContext: ExpressionParsingContext): Parser[Term] = Term.parser
  }

  def getRelation[T <: Expression : ChainingMethods](statement: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(BinaryJoiner[T], T, T)] = {
    implicitly[ChainingMethods[T]].getRelation(statement)
  }
  def getTransitivityStep[T <: Expression : ChainingMethods](
    source: T,
    intermediate: T,
    target: T,
    firstJoiner: BinaryJoiner[T],
    secondJoiner: BinaryJoiner[T])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[(BinaryJoiner[T], Step)] = {
    implicitly[ChainingMethods[T]].getTransitivityStep(source, intermediate, target, firstJoiner, secondJoiner)
  }
  def parser[T <: Expression : ChainingMethods](implicit expressionParsingContext: ExpressionParsingContext): Parser[T] = implicitly[ChainingMethods[T]].parser
}
