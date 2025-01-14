package net.prover.proving.structure.inferences

import net.prover.model.definitions.{Definitions, Wrapper, WrapperIdentity}
import net.prover.model.expressions._
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.proving.structure.statements.{BinaryConnective, BinaryJoiner, BinaryRelation}

// An inference that expands a simple binary joiner into a more complex one.
// The motivating inferences for this class are `a = b ⊢ F(a) = F(b)` and `a = b ⊢ φ(a) ↔ φ(b)`.
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

object Expansion {
  def fromInference(inference: Inference, definitions: Definitions): Option[Expansion[_ <: Expression]] = {
    implicit val substitutionContext: SubstitutionContext = SubstitutionContext.outsideProof
    for {
      premise <- inference.premises.single
      sourceRelation <- definitions.definedBinaryRelations.find(r => r.unapply(premise).nonEmpty)
      if premise == sourceRelation.withVariables(0, 1)
      expansion <- ConnectiveExpansion.fromInference(inference, premise, sourceRelation, definitions)
        .orElse(RelationExpansion.fromInference(inference, premise, sourceRelation, definitions))
    } yield expansion
  }
}

case class ConnectiveExpansion(sourceJoiner: BinaryRelation, resultJoiner: BinaryConnective, inference: Inference.Summary) extends Expansion[Statement] {
  override def wrapperIdentity: WrapperIdentity[Term, Statement] = WrapperIdentity.none
  override protected def getSubstitutions(left: Term, right: Term, wrapper: Wrapper[Term, Statement])(implicit substitutionContext: SubstitutionContext): Substitutions = {
    Substitutions(Seq(wrapper.template), Seq(left, right))
  }
}
object ConnectiveExpansion {
  // Find expansions of the form `a = b ⊢ F(a) = F(b)`
  def fromInference(inference: Inference, premise: Statement, sourceJoiner: BinaryRelation, definitions: Definitions)(implicit substitutionContext: SubstitutionContext): Option[ConnectiveExpansion] = {
    for {
      resultJoiner <- definitions.definedBinaryConnectives.find(_.unapply(inference.conclusion).nonEmpty)
      if inference.conclusion == resultJoiner(StatementVariable(0, Seq(TermVariable(0, Nil))), StatementVariable(0, Seq(TermVariable(1, Nil))))
    } yield ConnectiveExpansion(sourceJoiner, resultJoiner, inference.summary)
  }
}

case class RelationExpansion(sourceJoiner: BinaryRelation, resultJoiner: BinaryRelation, inference: Inference.Summary) extends Expansion[Term] {
  override def wrapperIdentity: WrapperIdentity[Term, Term] = implicitly
  override protected def getSubstitutions(left: Term, right: Term, wrapper: Wrapper[Term, Term])(implicit substitutionContext: SubstitutionContext): Substitutions = {
    Substitutions(Nil, Seq(left, right, wrapper.template))
  }
}
object RelationExpansion {
  // Find expansions of the form `a = b ⊢ φ(a) ↔ φ(b)`
  def fromInference(inference: Inference, premise: Statement, sourceJoiner: BinaryRelation, definitions: Definitions)(implicit substitutionContext: SubstitutionContext): Option[RelationExpansion] = {
    for {
      resultJoiner <- definitions.definedBinaryRelations.find(_.unapply(inference.conclusion).nonEmpty)
      if inference.conclusion == resultJoiner(TermVariable(2, Seq(TermVariable(0, Nil))), TermVariable(2, Seq(TermVariable(1, Nil))))
    } yield RelationExpansion(sourceJoiner, resultJoiner, inference.summary)
  }
}

