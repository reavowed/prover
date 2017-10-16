package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Term extends Expression {
  def increaseDepth(additionalDepth: Int, insertionPoint: Int): Term
  def reduceDepth(difference: Int, insertionPoint: Int): Option[Term]
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
    for {
      (argument, index) <- baseArguments.increaseDepth(depth - substitutions.depth, baseArguments.depth).terms.zipWithIndex
      updatedSubstitutions <- argument.calculateSubstitutions(this, substitutions)
    } yield FunctionParameter.anonymous(index, 1, depth - baseArguments.depth + 1) -> updatedSubstitutions
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
      case "with" =>
        for {
          arguments <- Term.parser.listOrSingle(None)
          name <- Parser.singleWord
        } yield FunctionApplication(name, ArgumentList(arguments, context.parameterDepth))
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
