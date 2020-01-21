package net.prover.controllers.models

import net.prover.model._
import net.prover.controllers._
import net.prover.model.expressions.{Statement, Term}

import scala.util.Try

case class SerializedSubstitutions(
  statements: Map[String, String],
  terms: Map[String, String],
  predicates: Map[String, Map[String, String]],
  functions: Map[String, Map[String, String]]
) {
  def parse()(implicit parsingContext: ExpressionParsingContext): Try[Substitutions] = {
    def lookup[T](
      source: Map[String, String],
      parser: ExpressionParsingContext => Parser[T],
      description: String)(
      implicit parsingContext: ExpressionParsingContext
    ): Try[Map[String, T]] = {
      source.map { case (name, serializedValue) =>
        Try(name -> parser(parsingContext).parseFromString(serializedValue, "")).orBadRequest(s"Invalid substitution $description $name '$serializedValue'")
      }.traverseTry.map(_.toMap)
    }
    def lookupWithPlaceholders[T](
      source: Map[String, Map[String, String]],
      parser: ExpressionParsingContext => Parser[T],
      description: String)(
      implicit parsingContext: ExpressionParsingContext
    ): Try[Map[(String, Int), T]] = {
      source.flatMap { case (name, valuesMap) =>
        valuesMap.map { case (numberOfParametersString, serializedValue) =>
          for {
            numberOfParameters <- numberOfParametersString.toInt.recoverWithBadRequest(_ => s"Invalid number of parameters $numberOfParametersString")
            value <- Try(parser(parsingContext.withPlaceholderParameters(numberOfParameters)).parseFromString(serializedValue, "")).orBadRequest(s"Invalid substitution $description $name '$serializedValue'")
          } yield ((name, numberOfParameters), value)
        }
      }.traverseTry.map(_.toMap)
    }

    for {
      statements <- lookup(statements, Statement.parser(_), "statement")
      terms <- lookup(terms, Term.parser(_), "term")
      predicates <- lookupWithPlaceholders(predicates, Statement.parser(_), "predicate")
      functions <- lookupWithPlaceholders(functions, Term.parser(_), "function")
    } yield Substitutions(statements, terms, predicates, functions)
  }
}
