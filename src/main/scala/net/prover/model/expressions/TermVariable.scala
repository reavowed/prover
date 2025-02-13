package net.prover.model.expressions

import net.prover.model._
import net.prover.parsing.KnownWordParser

case class TermVariable(index: Int, arguments: Seq[Term]) extends ExpressionVariable[Term] with ExpressionLenses.ForTerms with Term {
  def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case TermVariable(`index`, otherArguments) => Some(otherArguments)
    case _ => None
  }
  def update(newArguments: Seq[Term]): TermVariable = TermVariable(index, newArguments)

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
  def serializationPrefix: String = "t"
}

object TermVariable {
  def apply(index: Int): TermVariable = TermVariable(index, Nil)

  def simpleParser(implicit context: ExpressionParsingContext): KnownWordParser[TermVariable] = {
    context.simpleVariableParser(
      context.termVariableDefinitionParser,
      TermVariable(_, _),
      "term")
  }
  def applicationParser(implicit context: ExpressionParsingContext): KnownWordParser[TermVariable] = {
    context.variableApplicationParser(
      context.termVariableDefinitionParser,
      TermVariable(_, _),
      "term")
  }
}
