package net.prover.model.definitions

import net.prover.model.entries.DisplayShorthand
import net.prover.model.expressions.{Expression, Statement, Term, TermVariable}
import net.prover.model.proof.SubstitutionContext
import net.prover.model.{Inference, Substitutions}

sealed trait BinaryJoiner[TComponent <: Expression] extends Substitutions.Lenses[TComponent] {
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

  def reversal(inference: Inference.Summary): Reversal[TComponent] = Reversal[TComponent](this, inference)
}

sealed trait BinaryJoinerFromDefinition[TComponent <: Expression] extends BinaryJoiner[TComponent] {
  def definition: StatementDefinition
  override val symbol: String = definition.symbol
  override val template: Statement = definition.defaultValue
  override val attributes: Seq[String] = definition.attributes
}

case class BinaryConnective(definition: StatementDefinition) extends BinaryJoinerFromDefinition[Statement] with Substitutions.Lenses.ForStatements

sealed trait BinaryRelation extends BinaryJoiner[Term] with Substitutions.Lenses.ForTerms

case class BinaryRelationFromDefinition(definition: StatementDefinition) extends BinaryRelation with BinaryJoinerFromDefinition[Term]

case class BinaryRelationFromGeneralShorthand(definition: TermDefinition, shorthand: DisplayShorthand, lhsVariableName: String, rhsVariableName: String, symbolVariableName: String) extends BinaryRelation {
  override val symbol: String = definition.symbol
  override val template: Statement = shorthand.template.expand(
    Map.empty,
    Map(
      lhsVariableName -> TermVariable(lhsVariableName),
      rhsVariableName -> TermVariable(rhsVariableName),
      symbolVariableName -> definition.defaultValue)
  ).asInstanceOf[Statement]
  override val attributes: Seq[String] = Nil
}

case class BinaryRelationFromSpecificShorthand(symbol: String, shorthand: DisplayShorthand, lhsVariableName: String, rhsVariableName: String) extends BinaryRelation {
  override val template: Statement = shorthand.template.expand(
    Map.empty,
    Map(
      lhsVariableName -> TermVariable(lhsVariableName),
      rhsVariableName -> TermVariable(rhsVariableName))
  ).asInstanceOf[Statement]
  override val attributes: Seq[String] = Nil
}
