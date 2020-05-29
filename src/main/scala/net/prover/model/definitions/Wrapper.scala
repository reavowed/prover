package net.prover.model.definitions

import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext

class Wrapper[TInput, TOutput](f: (TInput, SubstitutionContext) => TOutput) {
  def apply(t: TInput)(implicit substitutionContext: SubstitutionContext): TOutput = f(t, substitutionContext)
  def insert[TNewInput](g: (TNewInput, SubstitutionContext) => TInput): Wrapper[TNewInput, TOutput] = new Wrapper((input, context) => f(g(input, context), context))
  def insertWrapper[TNewInput](wrapper: Wrapper[TNewInput, TInput]): Wrapper[TNewInput, TOutput] = new Wrapper((input, context) => f(wrapper(input)(context), context))
  def isIdentity(implicit wrapperIdentity: WrapperIdentity[TInput, TOutput], substitutionContext: SubstitutionContext): Boolean = wrapperIdentity.isIdentity(this, substitutionContext)
}

trait WrapperIdentity[T, S] {
  def isIdentity(wrapper: Wrapper[T, S], substitutionContext: SubstitutionContext): Boolean
}
object WrapperIdentity {
  implicit def termIdentity: WrapperIdentity[Term, Term] = (wrapper, context) => wrapper(TermVariable(Int.MaxValue))(context) == TermVariable(Int.MaxValue)
  implicit def statementIdentity: WrapperIdentity[Statement, Statement] = (wrapper, context) => wrapper(StatementVariable(Int.MaxValue))(context) == StatementVariable(Int.MaxValue)
  def none[T, S]: WrapperIdentity[T, S] = (_, _) => false
}

object Wrapper {
  def identity[T]: Wrapper[T, T] = new Wrapper((t, _) => t)
  def fromExpression[T <: Expression with TypedExpression[T]](expression: T): Wrapper[Term, T] = {
    new Wrapper((t, context) => expression.specify(Seq(t))(context).get)
  }

  implicit class TermWrapper[TOutput](wrapper: Wrapper[Term, TOutput]) {
    def template(implicit substitutionContext: SubstitutionContext): TOutput = {
      wrapper(FunctionParameter(0, substitutionContext.externalDepth))(SubstitutionContext.withExtraParameter)
    }
  }
}
