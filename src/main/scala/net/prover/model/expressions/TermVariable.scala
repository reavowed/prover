package net.prover.model.expressions

import net.prover.model._

case class TermVariable(name: String, arguments: Seq[Term]) extends ExpressionVariable[Term] with Substitutions.Lenses.ForTerms with Term {
  def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case TermVariable(`name`, otherArguments) => Some(otherArguments)
    case _ => None
  }
  def update(newArguments: Seq[Term]): TermVariable = TermVariable(name, newArguments)

  override def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, Term, Int, Seq[Int])] = {
    super[Term].getTerms(internalDepth, externalDepth) ++
      super[ExpressionVariable].getTerms(internalDepth, externalDepth)
  }
  override def getPredicateForTerm(term: Term, depth: Int): Term = {
    if (term == this)
      FunctionParameter(0, depth)
    else
      super.getPredicateForTerm(term, depth)
  }
  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(Term, Substitutions.Possible)] = {
    super[Term].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth) ++
      super[ExpressionVariable].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
  }
  def getSymbolForHash(variableDefinitions: VariableDefinitions): String = {
    "t" + variableDefinitions.termVariableDefinitions.findIndexWhere(d => d.name == name && d.arity == arguments.length)
      .getOrElse(throw new Exception(s"Could not find valid definition for variable $name"))
  }
}

object TermVariable {
  def apply(name: String): TermVariable = TermVariable(name, Nil)
}
