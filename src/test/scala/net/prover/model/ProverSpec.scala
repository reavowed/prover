package net.prover.model

import java.nio.file.Paths

import net.prover.model.components._
import net.prover.model.entries.{StatementDefinition, TermDefinition}
import net.prover.model.proof.Fact
import org.specs2.mutable.Specification

trait ProverSpec extends Specification {

  val φ = StatementVariable("φ")
  val ψ = StatementVariable("ψ")
  val χ = StatementVariable("χ")

  val x = TermVariable("x")
  val y = TermVariable("y")
  val z = TermVariable("z")
  val X = TermVariable("X")
  val Y = TermVariable("Y")
  val Z = TermVariable("Z")
  val a = TermVariable("a")
  val b = TermVariable("b")
  val n = TermVariable("n")

  def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val variables = Seq(φ, ψ, χ).take(size)
    StatementDefinition(
      symbol,
      Nil,
      variables,
      symbol,
      Format.default(symbol, variables.map(_.text)),
      definingStatement,
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
      Nil,
      variables,
      symbol,
      Format.default(symbol, variables.map(_.text)),
      definingStatement,
      "",
      "")
  }

  def quantifier(
    symbol: String,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq("x"),
      Seq(φ),
      symbol,
      Format(s"($symbol%0)%1", requiresBrackets = false),
      definingStatement,
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
  val ExistsUnique = quantifier("∃!", Some(Exists(y, ForAll(x, Equivalence(φ, Equals(x, y))))))
  val ElementOf = predicate("∈", 2, None)

  val EmptySetDefinition = TermDefinition(
    "∅",
    Nil,
    "∅",
    Format.default("∅", Nil),
    Nil,
    ForAll(x, Negation(ElementOf(x, PlaceholderTerm))),
    "",
    "")
  val EmptySet = EmptySetDefinition()

  val PowerSet = TermDefinition(
    "powerSet",
    Seq(x),
    "Power Set",
    Format("𝒫%0", requiresBrackets = false),
    Nil,
    ForAll(y, Equivalence(ElementOf(Y, PlaceholderTerm), φ)),
    "",
    "")

  implicit val defaultContext = ParsingContext(
    statementDefinitions = Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists, ExistsUnique,
      ElementOf, Equals),
    termDefinitions = Seq(EmptySetDefinition, PowerSet),
    statementVariableNames = Set(φ, ψ, χ).map(_.text),
    termVariableNames = Set(x, y, z, X, Y, Z, a, b, n).map(_.text),
    Seq.empty)

  val stubBook = Book("", Paths.get(""), Nil, Nil, Set.empty, Set.empty)
  val stubChapter = Chapter("", "", "")

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseAndDiscard(text, Paths.get(""))
    }
  }
  implicit class StatementDefinitionOps(statementDefinition: StatementDefinition) {
    def apply(components: Component*): DefinedStatement = {
      DefinedStatement(components, statementDefinition)(statementDefinition.boundVariableNames)
    }
    def apply(boundVariableNames: String*)(components: Component*): DefinedStatement = {
      DefinedStatement(components, statementDefinition)(boundVariableNames)
    }
  }

  trait PremiseConverter[-T] {
    def convertToPremise(t: T, index: Int): Premise
  }
  // An awkward little hack to allow Nil to work - you can't define a PremiseConverter[Nothing] as per
  // https://issues.scala-lang.org/browse/SI-4982.
  trait LowPriorityPremiseConverter {
    implicit val statementConverter: PremiseConverter[Statement] = (statement, index) => Premise(Fact.Direct(statement), index)(false)
  }
  object PremiseConverter extends LowPriorityPremiseConverter {
    implicit val factConverter: PremiseConverter[Fact] = (fact, index) => Premise(fact, index)(false)
  }
  implicit def allToPremise[T : PremiseConverter](ts: Seq[T]): Seq[Premise] = {
    val converter = implicitly[PremiseConverter[T]]
    ts.mapWithIndex(converter.convertToPremise)
  }
  implicit def statementToPredicate(statement: Statement): Predicate = Predicate.Constant(statement)
  implicit def termToFunction(term: Term): Function = Function.Constant(term)
}
