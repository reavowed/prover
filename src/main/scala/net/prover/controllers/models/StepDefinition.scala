package net.prover.controllers.models

import net.prover.model._
import net.prover.model.expressions.{Statement, Term}
import net.prover.controllers._

import scala.util.Try

case class StepDefinition(inferenceId: String, substitutions: StepDefinition.Substitutions) {
  def parseSubstitutions(inference: Inference, parsingContext: ParsingContext): Try[Substitutions] = {
    def lookup[T](
      required: Seq[String],
      source: Map[String, String],
      parser: ParsingContext => Parser[T],
      description: String)(
      parsingContext: ParsingContext
    ): Try[Map[String, T]] = {
      required.mapTryToMap { name =>
        for {
          input <- source.get(name).orBadRequest(s"Missing substitution $description $name")
          value <- Try(parser(parsingContext).parseFromString(input, "")).toOption.orBadRequest(s"Invalid substitution $description $name '$input'")
        } yield value
      }
    }
    for {
      statements <- lookup(inference.requiredSubstitutions.statements, substitutions.statements, Statement.parser(_), "statement")(parsingContext)
      terms <- lookup(inference.requiredSubstitutions.terms, substitutions.terms, Term.parser(_), "term")(parsingContext)
      predicates <- lookup(inference.requiredSubstitutions.predicates, substitutions.predicates, Statement.parser(_), "predicate")(parsingContext.withPlaceholderParameter())
      functions <- lookup(inference.requiredSubstitutions.functions, substitutions.functions, Term.parser(_), "function")(parsingContext.withPlaceholderParameter())
    } yield Substitutions(statements, terms, predicates, functions)
  }
}
object StepDefinition {
  case class Substitutions(statements: Map[String, String], terms: Map[String, String], predicates: Map[String, String], functions: Map[String, String])
}
