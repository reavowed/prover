package net.prover.model.definitions

import net.prover.model.entries.DisplayShorthand
import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext
import net.prover.model.{ExpressionLenses, Inference}

sealed trait BinaryJoiner[TComponent <: Expression] extends ExpressionLenses[TComponent] {
  def symbol: String
  def template: Statement
  def attributes: Seq[String]
  def apply(left: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Statement = {
    template.applySubstitutions(fillSubstitutions(Seq(left, right))).get
  }
  def unapply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(TComponent, TComponent)] = {
    for {
      substitutions <- template.calculateSubstitutions(statement)
      left <- getSubstitutions(substitutions).get(0)
      right <- getSubstitutions(substitutions).get(1)
    } yield (left, right)
  }

  def withVariables(firstIndex: Int, secondIndex: Int)(implicit substitutionContext: SubstitutionContext): Statement = {
    apply(createSimpleVariable(firstIndex), createSimpleVariable(secondIndex))
  }
  def reversal(inference: Inference.Summary, inferencePremise: Statement): Reversal[TComponent] = Reversal[TComponent](this, inference, inferencePremise)

  override def toString: String = symbol
}

sealed trait BinaryJoinerFromDefinition[TComponent <: Expression] extends BinaryJoiner[TComponent] {
  def definition: StatementDefinition
  def templateComponents: Seq[ExpressionVariable[TComponent]]
  override val symbol: String = definition.symbol
  override val template: Statement = definition(templateComponents:_*)
  override val attributes: Seq[String] = definition.attributes
}

case class BinaryConnective(definition: StatementDefinition) extends BinaryJoinerFromDefinition[Statement] with ExpressionLenses.ForStatements {
  override def templateComponents: Seq[StatementVariable] = Seq(StatementVariable(0), StatementVariable(1))
}

sealed trait BinaryRelation extends BinaryJoiner[Term] with ExpressionLenses.ForTerms

case class BinaryRelationFromDefinition(definition: StatementDefinition) extends BinaryRelation with BinaryJoinerFromDefinition[Term] {
  override def templateComponents: Seq[TermVariable] = Seq(TermVariable(0), TermVariable(1))
}

case class BinaryRelationFromGeneralShorthand(definition: TermDefinition, shorthand: DisplayShorthand, lhsVariableName: String, rhsVariableName: String, symbolVariableName: String) extends BinaryRelation {
  override val symbol: String = definition.symbol
  override val template: Statement = shorthand.template.expand(
    Map.empty,
    Map(
      lhsVariableName -> TermVariable(0),
      rhsVariableName -> TermVariable(1),
      symbolVariableName -> definition.defaultValue)
  ).asInstanceOf[Statement]
  override val attributes: Seq[String] = Nil
}

case class BinaryRelationFromSpecificShorthand(symbol: String, shorthand: DisplayShorthand, lhsVariableName: String, rhsVariableName: String) extends BinaryRelation {
  override val template: Statement = shorthand.template.expand(
    Map.empty,
    Map(
      lhsVariableName -> TermVariable(0),
      rhsVariableName -> TermVariable(1))
  ).asInstanceOf[Statement]
  override val attributes: Seq[String] = Nil
}
