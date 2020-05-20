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
  implicit def termIdentity: WrapperIdentity[Term, Term] = (wrapper, context) => wrapper(TermVariable("_"))(context) == TermVariable("_")
  implicit def statementIdentity: WrapperIdentity[Statement, Statement] = (wrapper, context) => wrapper(StatementVariable("_"))(context) == StatementVariable("_")
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
