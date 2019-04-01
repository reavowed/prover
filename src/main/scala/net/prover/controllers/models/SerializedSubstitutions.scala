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
  def parse(inference: Inference)(implicit parsingContext: ParsingContext): Try[Substitutions] = {
    def lookup[T](
      required: Seq[String],
      source: Map[String, String],
      parser: ParsingContext => Parser[T],
      description: String)(
      implicit parsingContext: ParsingContext
    ): Try[Map[String, T]] = {
      required.mapTryToMap { name =>
        for {
          input <- source.get(name).orBadRequest(s"Missing substitution $description $name")
          value <- Try(parser(parsingContext).parseFromString(input, "")).toOption.orBadRequest(s"Invalid substitution $description $name '$input'")
        } yield value
      }
    }
    def lookupWithPlaceholders[T](
      required: Seq[(String, Int)],
      source: Map[String, Map[String, String]],
      parser: ParsingContext => Parser[T],
      description: String)(
      implicit parsingContext: ParsingContext
    ): Try[Map[(String, Int), T]] = {
      required.mapTryToMap { case (name, numberOfParameters) =>
        for {
          input <- source.get(name).flatMap(_.get(numberOfParameters.toString)).orBadRequest(s"Missing substitution $description $name")
          value <- Try(parser(parsingContext.withPlaceholderParameters(numberOfParameters)).parseFromString(input, "")).toOption.orBadRequest(s"Invalid substitution $description $name '$input'")
        } yield value
      }
    }

    for {
      statements <- lookup(inference.requiredSubstitutions.statements, statements, Statement.parser(_), "statement")
      terms <- lookup(inference.requiredSubstitutions.terms, terms, Term.parser(_), "term")
      predicates <- lookupWithPlaceholders(inference.requiredSubstitutions.predicates, predicates, Statement.parser(_), "predicate")
      functions <- lookupWithPlaceholders(inference.requiredSubstitutions.functions, functions, Term.parser(_), "function")
    } yield Substitutions(statements, terms, predicates, functions)
  }
}
