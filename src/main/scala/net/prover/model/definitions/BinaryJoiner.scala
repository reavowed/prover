package net.prover.model.definitions

import net.prover.core.expressions._
import net.prover.core.substitutions.SubstitutionApplier
import net.prover.model.proof.SubstitutionContext
import net.prover.model.substitutions.SubstitutionCalculator
import net.prover.model.{ExpressionLenses, Inference}
import net.prover.structure.model.entries.DisplayShorthand

sealed trait BinaryJoiner[TComponent <: Expression] extends ExpressionLenses[TComponent] {
  def symbol: String
  def template: Statement
  def attributes: Seq[String]
  def apply(left: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Statement = {
    SubstitutionApplier.applySubstitutions(template, fillSubstitutions(Seq(left, right))).get
  }
  def unapply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(TComponent, TComponent)] = {
    for {
      substitutions <- SubstitutionCalculator.calculateSubstitutions(template, statement)
      left <- getSubstitutions(substitutions).get(0)
      right <- getSubstitutions(substitutions).get(1)
    } yield (left, right)
  }

  def reversal(inference: Inference.Summary): Reversal[TComponent] = Reversal[TComponent](this, inference)
}

sealed trait BinaryJoinerFromDefinition[TComponent <: Expression] extends BinaryJoiner[TComponent] {
  def definition: CompoundStatementDefinition
  def templateComponents: Seq[ExpressionVariable[_, TComponent]]
  override val symbol: String = definition.symbol
  override val template: Statement = definition(templateComponents:_*)
  override val attributes: Seq[String] = definition.attributes
}

case class BinaryConnective(definition: CompoundStatementDefinition) extends BinaryJoinerFromDefinition[Statement] with ExpressionLenses.ForStatements {
  override def templateComponents: Seq[StatementVariable] = Seq(StatementVariable.plain(0), StatementVariable.plain(1))
}

sealed trait BinaryRelation extends BinaryJoiner[Term] with ExpressionLenses.ForTerms

case class BinaryRelationFromDefinition(definition: CompoundStatementDefinition) extends BinaryRelation with BinaryJoinerFromDefinition[Term] {
  override def templateComponents: Seq[TermVariable] = Seq(TermVariable.plain(0), TermVariable.plain(1))
}

case class BinaryRelationFromGeneralShorthand(definition: CompoundTermDefinition, shorthand: DisplayShorthand, lhsVariableName: String, rhsVariableName: String, symbolVariableName: String) extends BinaryRelation {
  override val symbol: String = definition.symbol
  override val template: Statement = shorthand.template.expand(
    Map.empty,
    Map(
      lhsVariableName -> TermVariable.plain(0),
      rhsVariableName -> TermVariable.plain(1),
      symbolVariableName -> definition.defaultValue)
  ).asInstanceOf[Statement]
  override val attributes: Seq[String] = Nil
}

case class BinaryRelationFromSpecificShorthand(symbol: String, shorthand: DisplayShorthand, lhsVariableName: String, rhsVariableName: String) extends BinaryRelation {
  override val template: Statement = shorthand.template.expand(
    Map.empty,
    Map(
      lhsVariableName -> TermVariable.plain(0),
      rhsVariableName -> TermVariable.plain(1))
  ).asInstanceOf[Statement]
  override val attributes: Seq[String] = Nil
}
