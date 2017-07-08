package net.prover.model.components

import net.prover.model.{Context, Parser}

trait Variable extends Component {
  def text: String
}

object Variable {
  def parser(implicit context: Context): Parser[Variable] = {
    for {
      variableName <- Parser.singleWord
    } yield {
      context.statementVariableNames.find(_ == variableName).map(StatementVariable)
        .orElse(Term.findVariable(variableName))
        .getOrElse(throw new Exception(s"Unrecognised variable name '$variableName'"))
    }
  }
}
