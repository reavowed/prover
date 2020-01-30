package net.prover.model.definitions

import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.{Expression, Statement}
import net.prover.model.proof.SubstitutionContext

trait BinaryJoiner[TComponent <: Expression] extends Substitutions.Lenses[TComponent] {
  def symbol: String
  def template: Statement
  def attributes: Seq[String]
  def apply(left: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Statement = {
    template.applySubstitutions(fillRequiredSubstitutions(template.requiredSubstitutions, Seq(left, right))).get
  }
  def unapply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(TComponent, TComponent)] = {
    for {
      substitutions <- template.calculateSubstitutions(statement)
      Seq(left, right) <- getRequiredSubstitutions(substitutions, template.requiredSubstitutions)
    } yield (left, right)
  }

  def reversal(inference: Inference.Summary) = Reversal[TComponent](this, inference)
  def transitivity(inference: Inference.Summary) = Transitivity[TComponent](this, inference)
}
