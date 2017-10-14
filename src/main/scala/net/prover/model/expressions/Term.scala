package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Term extends Expression {
  def increaseDepth(additionalDepth: Int, insertionPoint: Int): Term
  def reduceDepth(difference: Int): Option[Term]
  def specify(arguments: ArgumentList): Term
  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions,
    outerDepth: Int
  ): Option[Term]
  def applySubstitutions(substitutions: Substitutions): Option[Term]
  def calculateApplicatives(
    baseArguments: ArgumentList,
    substitutions: Substitutions
  ): Seq[(Term, Substitutions)] = {
    baseArguments.terms.flatMapWithIndex { case (argument, index) =>
      argument.calculateSubstitutions(this, substitutions).map(FunctionParameter.anonymous(index, 1, depth - baseArguments.depth + 1) -> _)
    }
  }
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

    Parser.selectWordParser("term") {
      case context.RecognisedParameter(parameter) =>
        Parser.constant(parameter)
      case context.RecognisedTermDefinition(termDefinition) =>
        termDefinition.termParser
      case context.RecognisedTermVariable(name) =>
        Parser.constant(TermVariable(name, context.parameterDepth))
    }
  }

  def listParser(implicit context: ParsingContext): Parser[Seq[Term]] = {
    parser.listInParens(Some(","))
  }

  def variableParser(implicit context: ParsingContext): Parser[TermVariable] = parser.map(asVariable)

  def variableListParser(implicit context: ParsingContext): Parser[Seq[TermVariable]] = {
    variableParser.listInParens(None)
  }
}
