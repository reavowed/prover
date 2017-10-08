package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Function extends ExpressionFunction[Objectable] with Objectable {
  def applySubstitutions(substitutions: Substitutions): Option[Function]
  def replacePlaceholder(other: Expression): Function
  override def makeApplicative = None
}

object Function {
  def parser(implicit parsingContext: ParsingContext): Parser[Function] = {
    Parser.selectWord("function") {
      case "_" =>
        ConstantFunction(PlaceholderTerm, parsingContext.parameterDepth)
      case parsingContext.RecognisedParameter(parameter) =>
        parameter
      case parsingContext.RecognisedTermVariable(variable) =>
        ConstantFunction(variable, parsingContext.parameterDepth)
    }
  }
}


