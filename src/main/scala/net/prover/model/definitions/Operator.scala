package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext

sealed trait Operator {
  def template: Term
  protected def fill(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = {
    template.applySubstitutions(template.requiredSubstitutions.fill(Nil, terms)).get
  }
  protected def extract(t: Term)(implicit substitutionContext: SubstitutionContext): Option[Seq[Term]] = {
    for {
      substitutions <- template.calculateSubstitutions(t)
      result <- template.requiredSubstitutions.terms.map { case (name, _) => substitutions.terms.get(name).map(_._2) }.traverseOption
    } yield result
  }
}

case class UnaryOperator(template: Term) extends Operator {
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
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Term = {
    fill(Seq(left, right))
  }
  def unapply(term: Term)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = {
    for {
      Seq(left, right) <- extract(term)
    } yield (left, right)
  }
}
