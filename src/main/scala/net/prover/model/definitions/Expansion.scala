package net.prover.model.definitions

import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.{Premise, Step, SubstitutionContext}

trait Expansion[TComponent <: Expression] {
  def sourceJoiner: BinaryJoiner[Term]
  def resultJoiner: BinaryJoiner[TComponent]
  def inference: Inference.Summary
  implicit def wrapperIdentity: WrapperIdentity[Term, TComponent]
  protected def getSubstitutions(left: Term, right: Term, wrapper: Wrapper[Term, TComponent])(implicit substitutionContext: SubstitutionContext): Substitutions

  def assertionStep(left: Term, right: Term, wrapper: Wrapper[Term, TComponent])(implicit substitutionContext: SubstitutionContext): Step.AssertionStep = {
    Step.AssertionStep(
      resultJoiner(wrapper(left), wrapper(right)),
      inference,
      Seq(Premise.Pending(sourceJoiner(left, right))),
      getSubstitutions(left, right, wrapper))
  }

  def assertionStepIfNecessary(left: Term, right: Term, wrapper: Wrapper[Term, TComponent])(implicit substitutionContext: SubstitutionContext): Option[Step.AssertionStep] = {
    if (wrapper.isIdentity && sourceJoiner == resultJoiner) {
      None
    } else {
      Some(assertionStep(left, right, wrapper))
    }
  }
}

case class ConnectiveExpansion(sourceJoiner: BinaryRelation, resultJoiner: BinaryConnective, inference: Inference.Summary) extends Expansion[Statement] {
  override def wrapperIdentity: WrapperIdentity[Term, Statement] = WrapperIdentity.none
  override protected def getSubstitutions(left: Term, right: Term, wrapper: Wrapper[Term, Statement])(implicit substitutionContext: SubstitutionContext): Substitutions = {
    Substitutions(Seq(wrapper.template), Seq(left, right))
  }
}
case class RelationExpansion(sourceJoiner: BinaryRelation, resultJoiner: BinaryRelation, inference: Inference.Summary) extends Expansion[Term] {
  override def wrapperIdentity: WrapperIdentity[Term, Term] = implicitly
  override protected def getSubstitutions(left: Term, right: Term, wrapper: Wrapper[Term, Term])(implicit substitutionContext: SubstitutionContext): Substitutions = {
    Substitutions(Nil, Seq(left, right, wrapper.template))
  }
}

