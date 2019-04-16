package net.prover.model

import java.nio.file.Paths

import net.prover.model.entries.ExpressionDefinition.{ComponentType, StatementComponent, TermComponent}
import net.prover.model.expressions._
import net.prover.model.entries._
import org.specs2.mutable.Specification

trait ProverSpec extends Specification {

  val φ = StatementVariable("φ")
  val ψ = StatementVariable("ψ")
  val χ = StatementVariable("χ")

  val a = TermVariable("a")
  val b = TermVariable("b")
  val c = TermVariable("c")
  val n = TermVariable("n")
  val F = TermVariable("F")

  val stubBook = Book("", Nil, Nil, Nil)
  val stubChapter = Chapter("", "", Nil)

  def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val componentTypes = Seq[ComponentType](φ, ψ, χ).take(size)
    StatementDefinition(
      symbol,
      Nil,
      componentTypes,
      None,
      Format.default(symbol, componentTypes.map(_.name)),
      definingStatement,
      None,
      Nil)
  }
  def predicate(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val componentTypes = Seq[ComponentType](a, b, c).take(size)
    StatementDefinition(
      symbol,
      Nil,
      componentTypes,
      None,
      Format.default(symbol, componentTypes.map(_.name)),
      definingStatement,
      None,
      Nil)
  }

  def quantifier(
    symbol: String,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq("x"),
      Seq(ExpressionDefinition.PredicateComponent("φ", Seq(ExpressionDefinition.ComponentArgument("x", 0)))),
      None,
      Format.Explicit(s"($symbol%0)%1", s"(${symbol}x)φ", requiresBrackets = false, requiresComponentBrackets = true),
      definingStatement,
      None,
      Nil)
  }

  val Implication = connective("→", 2, None).copy(attributes = Seq("deduction"))
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(φ, Negation(ψ)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(φ), ψ)))
  val Equivalence = connective("↔", 2, None)

  val ForAll = quantifier("∀", None).copy(attributes = Seq("scoping"))
  val Exists = quantifier("∃", Some(Negation(ForAll("x")(Negation(φ(FunctionParameter(0, 0)))))))
  val Equals = predicate("=", 2, None)
  val ExistsUnique = quantifier("∃!", Some(Exists("y")(ForAll("x")(Equivalence(
    φ(FunctionParameter(0, 0)),
    Equals(FunctionParameter(0, 0), FunctionParameter(0, 1)))))))
  val ElementOf = predicate("∈", 2, None)
  val Subset = predicate("⊆", 2, Some(ForAll("x")(Implication(
      ElementOf(FunctionParameter(0, 0), a),
      ElementOf(FunctionParameter(0, 0), b)))))

  val EmptySetDefinition = TermDefinition(
    "∅",
    Nil,
    Nil,
    None,
    Format.default("∅", Nil),
    Nil,
    ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))),
    None,
    Nil)
  val EmptySet = DefinedTerm(Nil, EmptySetDefinition)(Nil)

  val PowerSet = TermDefinition(
    "powerSet",
    Nil,
    Seq(a),
    Some("Power Set"),
    Format.Explicit("𝒫%0", "𝒫a", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    ForAll("y")(Equivalence(
      ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)),
      Subset(FunctionParameter(0, 0), a))),
    None,
    Nil)

  val Singleton = TermDefinition(
    "singleton",
    Nil,
    Seq(a),
    Some("Singleton"),
    Format.Explicit("{%0}", "{a}", requiresBrackets = false, requiresComponentBrackets = false),
    Nil,
    φ,
    None,
    Nil)

  val Pair = TermDefinition(
    "pair",
    Nil,
    Seq(a, b),
    Some("Unordered Pair"),
    Format.Explicit("{%0, %1}", "{a, b}", requiresBrackets = false, requiresComponentBrackets = false),
    Nil,
    φ,
    None,
    Nil)

  implicit val entryContext: EntryContext = EntryContext(
    Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists, ExistsUnique,
      ElementOf, Equals, Subset) ++
      Seq(EmptySetDefinition, PowerSet, Singleton, Pair),
    Nil)
  implicit val parsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseFromString(text, "test")
    }
  }
  implicit class StatementVariableOps(statementVariable: StatementVariable) {
    def apply(terms: Term*) = PredicateApplication(statementVariable.name, terms)
  }
  implicit class TermVariableOps(termVariable: TermVariable) {
    def apply(terms: Term*) = FunctionApplication(termVariable.name, terms)
  }
  implicit def statementVariableToComponentType(statementVariable: StatementVariable): StatementComponent = StatementComponent(statementVariable.name)
  implicit def termVariableToComponentType(termVariable: TermVariable): TermComponent = TermComponent(termVariable.name)
  implicit def variableTupleToString[T](tuple: (ExpressionVariable[_], T)): (String, T) = tuple.mapLeft(_.name)
  implicit def variableTupleTupleToString[T](tuple: ((ExpressionVariable[_], Int), T)): ((String, Int), T) = tuple.mapLeft(_.mapLeft(_.name))
  implicit class StatementDefinitionOps(statementDefinition: StatementDefinition) {
    def apply(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition)(statementDefinition.boundVariableNames)
    }
    def apply(boundVariableNames: String*)(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition)(boundVariableNames)
    }
  }
  implicit class TermDefinitionOps(termDefinition: TermDefinition) {
    def apply(components: Expression*): DefinedTerm = {
      DefinedTerm(components, termDefinition)(termDefinition.boundVariableNames)
    }
    def apply(boundVariableNames: String*)(components: Expression*): DefinedTerm = {
      DefinedTerm(components, termDefinition)(boundVariableNames)
    }
  }
}
