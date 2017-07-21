package net.prover.model

import java.nio.file.Paths

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}
import net.prover.model.components._
import net.prover.model.entries.{StatementDefinition, TermDefinition}
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
  def n = TermVariable("n")

  def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val variables = Seq(φ, ψ, χ).take(size)
    StatementDefinition(
      symbol,
      variables,
      symbol,
      Format.default(symbol, variables.map(_.text)),
      definingStatement,
      Set.empty,
      DistinctVariables.empty,
      "",
      "")
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
      symbol,
      Format.default(symbol, variables.map(_.text)),
      definingStatement,
      Set.empty,
      DistinctVariables.empty,
      "",
      "")
  }

  def quantifier(
    symbol: String,
    definingStatement: Option[Statement],
    distinctVariables: DistinctVariables = DistinctVariables.empty
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq(x, φ),
      symbol,
      Format(s"($symbol%0)%1", requiresBrackets = false),
      definingStatement,
      Set(x),
      distinctVariables,
      "",
      "")
  }

  val Implication = connective("→", 2, None)
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(φ, Negation(ψ)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(φ), ψ)))
  val Equivalence = connective("↔", 2, None)

  val ForAll = quantifier("∀", None)
  val Exists = quantifier("∃", Some(Negation(ForAll(x, Negation(φ)))))
  val Equals = predicate("=", 2, None)
  val ExistsUnique = quantifier("∃!", Some(Exists(y, ForAll(x, Equivalence(φ, Equals(x, y))))), DistinctVariables(x -> y))
  val ElementOf = predicate("∈", 2, None)

  val EmptySetDefinition = TermDefinition(
    "∅",
    Nil,
    "∅",
    Format.default("∅", Nil),
    Nil,
    ForAll(x, Negation(ElementOf(x, PlaceholderTerm))),
    Set.empty,
    DistinctVariables.empty,
    "",
    "")
  val EmptySet = EmptySetDefinition()

  val Comprehension = TermDefinition(
    "comprehension",
    Seq(x, y, φ),
    "Set Comprehension",
    Format("{%0 ∈ %1 | %2}", requiresBrackets = false),
    Nil,
    ForAll(z, Equivalence(ElementOf(z, PlaceholderTerm), Conjunction(ElementOf(z, y), φ.sub(z, x)))),
    Set(x),
    DistinctVariables.empty,
    "",
    "")

  val PowerSet = TermDefinition(
    "powerSet",
    Seq(x),
    "Power Set",
    Format("𝒫%0", requiresBrackets = false),
    Nil,
    ForAll(y, Equivalence(ElementOf(Y, PlaceholderTerm), φ)),
    Set.empty,
    DistinctVariables.empty,
    "",
    "")

  val Pair = TermDefinition(
    "pair",
    Seq(x, y),
    "Unordered Pair",
    Format("{%0, %1}", requiresBrackets = false),
    Nil,
    φ,
    Set.empty,
    DistinctVariables.empty,
    "",
    "")

  val OrderedPair = TermDefinition(
    "orderedPair",
    Seq(x, y),
    "Ordered Pair",
    Format("(%0, %1)", requiresBrackets = false),
    Nil,
    φ,
    Set.empty,
    DistinctVariables.empty,
    "",
    "")

  val Union = TermDefinition(
    "union",
    Seq(x),
    "Arbitrary Union",
    Format("⋃%0", requiresBrackets = true),
    Nil,
    φ,
    Set.empty,
    DistinctVariables.empty,
    "",
    "")

  implicit val defaultContext = ParsingContext(
    statementDefinitions = Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists, ExistsUnique,
      ElementOf, Equals),
    termDefinitions = Seq(EmptySetDefinition, Comprehension, PowerSet, Pair, OrderedPair, Union),
    statementVariableNames = Set(φ, ψ, χ).map(_.text),
    termVariableNames = Set(x, y, z, X, Y, Z, a, n).map(_.text))

  val stubBook = Book("", Paths.get(""), Nil, Nil, Set.empty, Set.empty)
  val stubChapter = Chapter("", "", "")

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseAndDiscard(text, Paths.get(""))
    }
  }

  implicit def statementToPremise(statement: Statement): Premise = DirectPremise(statement)
  implicit def statementPairToPremise(tuple: (Statement, Statement)): Premise = DeducedPremise(tuple._1, tuple._2)
  implicit def statementToProvenStatement(statement: Statement): ProvenStatement = ProvenStatement.withNoConditions(statement)
}
