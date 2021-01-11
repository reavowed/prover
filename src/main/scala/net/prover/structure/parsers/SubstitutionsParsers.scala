package net.prover.structure.parsers

import net.prover.core.substitutions.Substitutions
import net.prover.model.{ExpressionParsingContext, Inference, Parser}
import StatementParsers._
import TermParsers._

object SubstitutionsParsers {

  def substitutionsParser(inference: Inference)(implicit parsingContext: ExpressionParsingContext): Parser[Substitutions] = {
    for {
      statements <- inference.variableDefinitions.statements.map(d => statementParser(parsingContext.addInitialParameters(d.arity))).inParens(Some(","))
      terms <- inference.variableDefinitions.terms.map(d => termParser(parsingContext.addInitialParameters(d.arity))).inParens(Some(","))
    } yield {
      if (statements.length != inference.variableDefinitions.statements.length) {
        throw new Exception(s"Invalid number of statements in substitutions - expected ${inference.variableDefinitions.statements.length}, got ${statements.length}")
      }
      if (terms.length != inference.variableDefinitions.terms.length) {
        throw new Exception(s"Invalid number of terms in substitutions - expected ${inference.variableDefinitions.terms.length}, got ${terms.length}")
      }
      Substitutions(statements, terms)
    }
  }
}
