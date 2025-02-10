package net.prover.controllers.models

import net.prover.controllers._
import net.prover.model._
import net.prover.model.expressions.{Statement, Term}
import net.prover.parsing.Parser

import scala.util.Try

case class SerializedSubstitutions(statements: Seq[String], terms: Seq[String]) {
  def parse(variableDefinitions: VariableDefinitions)(implicit parsingContext: ExpressionParsingContext): Try[Substitutions] = {
    def lookup[T](
      serializedValues: Seq[String],
      definitions: Seq[VariableDefinition],
      parser: ExpressionParsingContext => Parser[T],
      description: String)(
      implicit parsingContext: ExpressionParsingContext
    ): Try[Seq[T]] = {
      for {
        serializedValuesWithDefinition <- serializedValues.zipStrict(definitions).orBadRequest(s"Invalid number of ${description}s - expected ${definitions.length}, got ${serializedValues.length}")
        values <- serializedValuesWithDefinition.zipWithIndex.map { case ((value, definition), index) =>
          parser(parsingContext.withPlaceholderParameters(definition.arity)).parseFromString(value, s"substitution $description $index").recoverWithBadRequest
        }.traverseTry
      } yield values
    }

    for {
      statements <- lookup(statements, variableDefinitions.statements,  Statement.parser(_), "statement")
      terms <- lookup(terms, variableDefinitions.terms, Term.parser(_), "term")
    } yield Substitutions(statements, terms)
  }
}
