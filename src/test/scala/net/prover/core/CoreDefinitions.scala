package net.prover.core

import net.prover.core.expressions._
import net.prover.core.proof.{ImplicationIntroductionStep, Step}

object CoreDefinitions {
  trait Placeholder[TVariable <: ExpressionVariable[TVariable, TExpression], TExpression <: Expression] {
    def name: String
    def index: Int
    def toVariable: TVariable
  }
  case class StatementVariablePlaceholder(name: String, index: Int) extends Placeholder[StatementVariable, Statement] {
    def apply(terms: Term*) = StatementVariable(index, terms)
    override def toVariable = apply()
    def ->(statement: Statement): (Statement, Statement) = toVariable -> statement
  }
  case class TermVariablePlaceholder(name: String, index: Int) extends Placeholder[TermVariable, Term] {
    def apply(terms: Term*) = TermVariable(index, terms)
    override def toVariable = apply()
    def ->(term: Term): (Term, Term) = toVariable -> term
  }
  implicit def placeholderToVariable[TVariable <: ExpressionVariable[TVariable, TExpression], TExpression <: Expression](placeholder: Placeholder[TVariable, TExpression]): TVariable = placeholder.toVariable

  val φ = StatementVariablePlaceholder("φ", 0)
  val ψ = StatementVariablePlaceholder("ψ", 1)
  val χ = StatementVariablePlaceholder("χ", 2)
  val ω = StatementVariablePlaceholder("ω", 3)


  case class Connective(symbol: String) extends CompoundStatementType {
    override def hasBoundVariables = false
    def apply(first: Statement, second: Statement): CompoundStatement = CompoundStatement(this, Seq(first, second))(Nil)
  }

  val Implies = Connective("→")

  def implicationStep(antecedent: Statement, firstStep: Step, subsequentSteps: Step*): ImplicationIntroductionStep = ImplicationIntroductionStep(antecedent, ::(firstStep, subsequentSteps.toList), ImplicationDefinition(Implies))

  val ModusPonens = RuleOfInference.Raw(Seq(Implies(φ, ψ), φ), ψ)
}
