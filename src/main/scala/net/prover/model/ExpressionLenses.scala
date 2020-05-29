package net.prover.model

import monocle.Lens
import monocle.macros.GenLens
import net.prover.model.Substitutions.Possible
import net.prover.model.expressions.{Expression, Statement, Term}

trait ExpressionLenses[TExpression <: Expression] {
  def variableDefinitionsLens: Lens[VariableDefinitions, Seq[VariableDefinition]]
  def usedVariablesLens: Lens[UsedVariables, Seq[UsedVariable]]
  def substitutionsLens: Lens[Substitutions, Seq[TExpression]]
  def possibleSubstitutionsLens: Lens[Substitutions.Possible, Map[Int, TExpression]]
  def possibleSubstitutionsApplicationsLens: Lens[Substitutions.Possible, Map[Int, Seq[(Seq[Term], TExpression, Int)]]]

  def fillSubstitutions(expressions: Seq[TExpression]): Substitutions = {
    substitutionsLens.set(expressions)(Substitutions.empty)
  }
  def getSubstitutions(possible: Possible): Map[Int, TExpression] = {
    possibleSubstitutionsLens.get(possible)
  }
}

object ExpressionLenses {
  def apply[T <: Expression](implicit expressionLenses: ExpressionLenses[T]): ExpressionLenses[T] = expressionLenses
  trait ForStatements extends ExpressionLenses[Statement] {
    override def variableDefinitionsLens = GenLens[VariableDefinitions](_.statements)
    override def usedVariablesLens = GenLens[UsedVariables](_.statements)
    override def substitutionsLens = GenLens[Substitutions](_.statements)
    override def possibleSubstitutionsLens = Possible.statementsLens
    override def possibleSubstitutionsApplicationsLens = Possible.statementApplicationsLens
  }
  implicit object ForStatements extends ForStatements
  trait ForTerms extends ExpressionLenses[Term] {
    override def variableDefinitionsLens = GenLens[VariableDefinitions](_.terms)
    override def usedVariablesLens = GenLens[UsedVariables](_.terms)
    override def substitutionsLens = GenLens[Substitutions](_.terms)
    override def possibleSubstitutionsLens = Possible.termsLens
    override def possibleSubstitutionsApplicationsLens = Possible.termApplicationsLens
  }
  implicit object ForTerms extends ForTerms
}
