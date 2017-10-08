package net.prover.model.expressions

import net.prover.model.entries.TermDefinition
import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Term extends Objectable {
  def depth: Int = 0
  def replacePlaceholder(other: Expression): Term
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
