package net.prover.model.components

import net.prover.model.{Parser, ParsingContext}

trait Variable extends Component {
  def text: String
}

object Variable {
  def parser(implicit context: ParsingContext): Parser[Variable] = {
    for {
      variableName <- Parser.singleWord
    } yield {
      context.statementVariableNames.find(_ == variableName).map(StatementVariable)
        .orElse(context.RecognisedTermVariable.unapply(variableName))
        .getOrElse(throw new Exception(s"Unrecognised variable name '$variableName'"))
    }
  }
}
