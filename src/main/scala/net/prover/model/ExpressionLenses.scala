package net.prover.model

import monocle.Lens
import monocle.macros.GenLens
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.substitutionFinding.model.PossibleSubstitutions

trait ExpressionLenses[TExpression <: Expression] {
  def variableDefinitionsLens: Lens[VariableDefinitions, Seq[VariableDefinition]]
  def usedVariablesLens: Lens[UsedVariables, Seq[UsedVariable]]
  def substitutionsLens: Lens[Substitutions, Seq[TExpression]]
  def possibleSubstitutionsLens: Lens[PossibleSubstitutions, Map[Int, TExpression]]
  def possibleSubstitutionsApplicationsLens: Lens[PossibleSubstitutions, Map[Int, Seq[(Seq[Term], TExpression, Int)]]]

  def fillSubstitutions(expressions: Seq[TExpression]): Substitutions = {
    substitutionsLens.set(expressions)(Substitutions.empty)
  }
  def getSubstitutions(possible: PossibleSubstitutions): Map[Int, TExpression] = {
    possibleSubstitutionsLens.get(possible)
  }
}

object ExpressionLenses {
  def apply[T <: Expression](implicit expressionLenses: ExpressionLenses[T]): ExpressionLenses[T] = expressionLenses
  trait ForStatements extends ExpressionLenses[Statement] {
    override def variableDefinitionsLens = GenLens[VariableDefinitions](_.statements)
    override def usedVariablesLens = GenLens[UsedVariables](_.statements)
    override def substitutionsLens = GenLens[Substitutions](_.statements)
    override def possibleSubstitutionsLens = PossibleSubstitutions.statementsLens
    override def possibleSubstitutionsApplicationsLens = PossibleSubstitutions.statementApplicationsLens
  }
  implicit object ForStatements extends ForStatements
  trait ForTerms extends ExpressionLenses[Term] {
    override def variableDefinitionsLens = GenLens[VariableDefinitions](_.terms)
    override def usedVariablesLens = GenLens[UsedVariables](_.terms)
    override def substitutionsLens = GenLens[Substitutions](_.terms)
    override def possibleSubstitutionsLens = PossibleSubstitutions.termsLens
    override def possibleSubstitutionsApplicationsLens = PossibleSubstitutions.termApplicationsLens
  }
  implicit object ForTerms extends ForTerms
}
