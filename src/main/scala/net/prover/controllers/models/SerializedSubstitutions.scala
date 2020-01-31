package net.prover.controllers.models

import net.prover.model._
import net.prover.controllers._
import net.prover.model.expressions.{Statement, Term}

import scala.util.Try

case class SerializedSubstitutions(
  statements: Map[String, (Int, String)],
  terms: Map[String, (Int, String)]
) {
  def parse()(implicit parsingContext: ExpressionParsingContext): Try[Substitutions] = {
    def lookup[T](
      source: Map[String, (Int, String)],
      parser: ExpressionParsingContext => Parser[T],
      description: String)(
      implicit parsingContext: ExpressionParsingContext
    ): Try[Map[String, (Int, T)]] = {
      source.map { case (name, (arity, serializedValue)) =>
        for {
          value <- Try(parser(parsingContext.withPlaceholderParameters(arity)).parseFromString(serializedValue, "")).orBadRequest(s"Invalid substitution $description $name '$serializedValue'")
        } yield (name, (arity, value))
      }.traverseTry.map(_.toMap)
    }

    for {
      statements <- lookup(statements, Statement.parser(_), "statement")
      terms <- lookup(terms, Term.parser(_), "term")
    } yield Substitutions(statements, terms)
  }
}
