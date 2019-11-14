package net.prover.model.definitions

import net.prover.model.expressions._
import net.prover.model.proof.SubstitutionContext

class Wrapper[TInput, TOutput](f: TInput => TOutput) {
  def apply(t: TInput): TOutput = f(t)
  def insert[TNewInput](g: TNewInput => TInput): Wrapper[TNewInput, TOutput] = new Wrapper(f compose g)
}
object Wrapper {
  def identity[T]: Wrapper[T, T] = new Wrapper(scala.Predef.identity)
  def fromFunction(function: Term)(implicit substitutionContext: SubstitutionContext): Wrapper[Term, Term] = {
    new Wrapper(t => function.specify(Seq(t)))
  }

  implicit class TermWrapper[TOutput](wrapper: Wrapper[Term, TOutput]) {
    def template(implicit substitutionContext: SubstitutionContext): TOutput = {
      wrapper(FunctionParameter(0, substitutionContext.externalDepth))
    }
  }
  implicit class TermTermWrapper(wrapper: Wrapper[Term, Term]) {
    def isIdentity: Boolean = wrapper(TermVariable("_")) == TermVariable("_")
  }
  implicit class StatementStatementWrapper(wrapper: Wrapper[Statement, Statement]) {
    def isIdentity: Boolean = wrapper(StatementVariable("_")) == StatementVariable("_")
  }
}
