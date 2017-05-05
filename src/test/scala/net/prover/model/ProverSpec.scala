package net.prover.model

import java.nio.file.Paths

import net.prover.model.Inference.{DirectPremise, Premise}
import org.specs2.mutable.Specification

trait ProverSpec extends Specification {

  def φ = StatementVariable("φ")
  def ψ = StatementVariable("ψ")
  def χ = StatementVariable("χ")

  def x = TermVariable("x")
  def y = TermVariable("y")
  def z = TermVariable("z")
  def X = TermVariable("X")
  def Y = TermVariable("Y")
  def Z = TermVariable("Z")
  def a = TermVariable("a")

  def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val variables = Seq(φ, ψ, χ).take(size)
    StatementDefinition(
      symbol,
      variables,
      Format.default(symbol, variables.map(_.text)),
      Set.empty,
      Map.empty,
      definingStatement)
  }
  def predicate(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val variables = Seq(x, y, z).take(size)
    StatementDefinition(
      symbol,
      variables,
      Format.default(symbol, variables.map(_.text)),
      Set.empty,
      Map.empty,
      definingStatement)
  }

  def quantifier(
    symbol: String,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq(x, φ),
      Format(s"($symbol%0)%1", requiresBrackets = false),
      Set(x),
      Map.empty,
      definingStatement)
  }


  val Implication = connective("→", 2, None)
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(φ, Negation(ψ)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(φ), ψ)))
  val Equivalence = connective("↔", 2, None)

  val ForAll = quantifier("∀", None)
  val Exists = quantifier("∃", Some(Negation(ForAll(x, Negation(φ)))))
  val ExistsUnique = quantifier("∃!", None)
  val ElementOf = predicate("∈", 2, None)
  val Equals = predicate("=", 2, None)

  val EmptySetDefinition = TermDefinition(
    "∅",
    Nil,
    "∅",
    Format.default("∅", Nil),
    Nil,
    ForAll(x, Negation(ElementOf(x, PlaceholderTerm))))
  val EmptySet = DefinedTerm(Nil, EmptySetDefinition)

  val statementDefinitions = Seq(
    Implication, Negation, Conjunction, Disjunction, Equivalence,
    ForAll, Exists, ExistsUnique,
    ElementOf, Equals)

  val baseContext = Context.empty.copy(variables = Variables(
    Set(φ, ψ, χ),
    Set(x, y, z, X, Y, Z, a)))

  implicit val defaultContext = statementDefinitions.foldLeft(baseContext) { case (context, statementDefinition) =>
    context.addStatementDefinition(statementDefinition)
  }.addTermDefinition(EmptySetDefinition)

  def contextWith(inferences: Inference*): Context = {
    defaultContext.copy(inferences = inferences)
  }

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parse(Tokenizer.fromString(text, Paths.get("")))._1
    }
  }

  implicit def statementToPremise(statement: Statement): Premise = DirectPremise(statement)
  implicit def statementToProvenStatement(statement: Statement): ProvenStatement = ProvenStatement.withNoConditions(statement)
}
