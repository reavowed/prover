package net.prover.model.expressions

import net.prover.model.entries.TermDefinition
import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Term extends Expression {
  def applySubstitutions(substitutions: Substitutions): Option[Term]
  def replacePlaceholder(other: Expression): Term
  def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int): Seq[(Function, Substitutions)] = {
    argument.calculateSubstitutions(this, substitutions, boundVariableCount).map(Function.Identity -> _)
  }
  def makeApplicative(argument: Term): Option[Term] = None
}

object Term {
  def asVariable(expression: Expression): TermVariable = {
    optionAsVariable(expression).getOrElse(throw new Exception(s"Expected term variable, got $expression"))
  }

  def optionAsVariable(expression: Expression): Option[TermVariable] = {
    expression match {
      case v: TermVariable =>
        Some(v)
      case _ =>
        None
    }
  }

  def parser(implicit context: ParsingContext): Parser[Term] = {
    object TermDefinitionMatcher {
      def unapply(s: String): Option[TermDefinition] = {
        context.termDefinitions.find(_.symbol == s)
      }
    }

    Parser.selectWordParser("term") {
      case "_" =>
        Parser.constant(PlaceholderTerm)
      case context.RecognisedBoundVariable(variable) =>
        Parser.constant(variable)
      case TermDefinitionMatcher(termDefinition) =>
        termDefinition.termParser
      case context.RecognisedTermVariable(variable) =>
        Parser.constant(variable)
    }
  }

  def listParser(implicit context: ParsingContext): Parser[Seq[Term]] = {
    parser.listInParens(Some(","))
  }

  def variableParser(implicit context: ParsingContext): Parser[TermVariable] = parser.map(asVariable)

  def variableListParser(implicit context: ParsingContext): Parser[Seq[TermVariable]] = {
    variableParser.listInParens(None)
  }

  def applicativeParser(implicit context: ParsingContext) = Function.parser
}
