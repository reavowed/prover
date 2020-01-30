package net.prover.model.definitions

import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext

class Wrapper[TInput, TOutput](f: TInput => TOutput) {
  def apply(t: TInput): TOutput = f(t)
  def insert[TNewInput](g: TNewInput => TInput): Wrapper[TNewInput, TOutput] = new Wrapper(f compose g)
  def isIdentity(implicit wrapperIdentity: WrapperIdentity[TInput, TOutput]): Boolean = wrapperIdentity.isIdentity(this)
}

trait WrapperIdentity[T, S] {
  def isIdentity(wrapper: Wrapper[T, S]): Boolean
}
trait LowPriorityWrapperIdentity {
  implicit def notIdentity[T, S]: WrapperIdentity[T, S] = (_: Wrapper[T, S]) => false
}
object WrapperIdentity extends LowPriorityWrapperIdentity {
  implicit def termIdentity: WrapperIdentity[Term, Term] = (wrapper: Wrapper[Term, Term]) => wrapper(TermVariable("_")) == TermVariable("_")
  implicit def statementIdentity: WrapperIdentity[Statement, Statement] = (wrapper: Wrapper[Statement, Statement]) => wrapper(StatementVariable("_")) == StatementVariable("_")
}

object Wrapper extends LowPriorityWrapperIdentity {
  def identity[T]: Wrapper[T, T] = new Wrapper(scala.Predef.identity)
  def fromExpression[T <: Expression with TypedExpression[T]](expression: T)(implicit substitutionContext: SubstitutionContext): Wrapper[Term, T] = {
    new Wrapper(t => expression.specify(Seq(t)).get)
  }

  implicit class TermWrapper[TOutput](wrapper: Wrapper[Term, TOutput]) {
    def template(implicit substitutionContext: SubstitutionContext): TOutput = {
      wrapper(FunctionParameter(0, substitutionContext.externalDepth))
    }
  }
}
