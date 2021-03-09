package net.prover.model.definitions

import net.prover.model.{ExpressionLenses, ProvingContext}
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext
import net.prover.old.OldSubstitutionApplier
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator

sealed trait Operator {
  def arity: Int
  def template: Term
  protected def fill(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = {
    OldSubstitutionApplier.applySubstitutions(template, ExpressionLenses.ForTerms.fillSubstitutions(terms)).get
  }
  protected def extract(t: Term)(implicit substitutionContext: SubstitutionContext): Option[Seq[Term]] = {
    for {
      substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(template, t)
      terms <- (0 until arity).map(substitutions.terms.get).traverseOption
    } yield terms
  }
}

case class UnaryOperator(template: Term) extends Operator {
  val arity = 1
  def apply(t: Term)(implicit substitutionContext: SubstitutionContext): Term = {
    fill(Seq(t))
  }
  def unapply(term: Term)(implicit substitutionContext: SubstitutionContext): Option[Term] = {
    for {
      Seq(t) <- extract(term)
    } yield t
  }
}
object UnaryOperator {
  def unapply(term: Term)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(UnaryOperator, Term)] = provingContext.unaryOperators.mapFind { o =>
    o.unapply(term).map { o -> _ }
  }
}

case class BinaryOperator(template: Term) extends Operator {
  val arity = 2
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Term = {
    fill(Seq(left, right))
  }
  def unapply(term: Term)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = {
    for {
      Seq(left, right) <- extract(term)
    } yield (left, right)
  }
}
