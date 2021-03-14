package net.prover.model.definitions

import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext
import net.prover.old.ExpressionSpecifier

sealed trait Wrapper[TInput, TOutput] {
  def apply(t: TInput)(implicit substitutionContext: SubstitutionContext): TOutput
  def insert[TNewInput](f: (TNewInput, SubstitutionContext) => TInput): Wrapper[TNewInput, TOutput] = ChainedWrapper(this, BaseWrapper(f))
  def insertWrapper[TNewInput](innerWrapper: Wrapper[TNewInput, TInput]): Wrapper[TNewInput, TOutput] = ChainedWrapper(this, innerWrapper)
  def isIdentity(implicit wrapperIdentity: WrapperIdentity[TInput, TOutput], substitutionContext: SubstitutionContext): Boolean = wrapperIdentity.isIdentity(this, substitutionContext)
}
object Wrapper {
  def apply[TInput, TOutput](f: (TInput, SubstitutionContext) => TOutput): Wrapper[TInput, TOutput] = BaseWrapper(f)
  def identity[T]: Wrapper[T, T] = Wrapper((t, _) => t)
  def fromExpression[T <: Expression with TypedExpression[T]](expression: T): Wrapper[Term, T] = {
    Wrapper((t, context) => ExpressionSpecifier.specify(expression, Seq(t))(context).get.asInstanceOf[T])
  }
  implicit class TermWrapper[TOutput](wrapper: Wrapper[Term, TOutput]) {
    def template(implicit substitutionContext: SubstitutionContext): TOutput = {
      wrapper(FunctionParameter(0, substitutionContext.externalDepth))(SubstitutionContext.withExtraParameter)
    }
  }
}

case class BaseWrapper[TInput, TOutput](f: (TInput, SubstitutionContext) => TOutput) extends Wrapper[TInput, TOutput] {
  def apply(t: TInput)(implicit substitutionContext: SubstitutionContext): TOutput = f(t, substitutionContext)
}
case class ChainedWrapper[TInput, TOriginalInput, TOutput](outerWrapper: Wrapper[TOriginalInput, TOutput], innerWrapper: Wrapper[TInput, TOriginalInput]) extends Wrapper[TInput, TOutput] {
  def apply(t: TInput)(implicit substitutionContext: SubstitutionContext): TOutput = outerWrapper(innerWrapper(t))
}


trait WrapperIdentity[T, S] {
  def isIdentity(wrapper: Wrapper[T, S], substitutionContext: SubstitutionContext): Boolean
}
object WrapperIdentity {
  implicit def termIdentity: WrapperIdentity[Term, Term] = (wrapper, context) => wrapper(TermVariable(Int.MaxValue))(context) == TermVariable(Int.MaxValue)
  implicit def statementIdentity: WrapperIdentity[Statement, Statement] = (wrapper, context) => wrapper(StatementVariable(Int.MaxValue))(context) == StatementVariable(Int.MaxValue)
  def none[T, S]: WrapperIdentity[T, S] = (_, _) => false
}
