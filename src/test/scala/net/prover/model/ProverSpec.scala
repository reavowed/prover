package net.prover.model

import net.prover.model.definitions.Definitions
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries._
import net.prover.model.expressions._
import net.prover.model.proof.{StepContext, StepProvingContext}
import org.specs2.mutable.Specification

trait ProverSpec extends Specification {

  val φ = StatementVariable("φ")
  val ψ = StatementVariable("ψ")
  val χ = StatementVariable("χ")

  val a = TermVariable("a")
  val b = TermVariable("b")
  val c = TermVariable("c")
  val d = TermVariable("d")
  val A = TermVariable("A")
  val B = TermVariable("B")
  val C = TermVariable("C")
  val D = TermVariable("D")
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
      Seq(ComponentType.PredicateComponent("φ", Seq(ExpressionDefinition.ComponentArgument("x", 0)))),
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
  val Equivalence = connective("↔", 2, Some(Conjunction(Implication(φ, ψ), Implication(ψ, φ))))

  val ForAll = quantifier("∀", None).copy(attributes = Seq("scoping"))
  val Exists = quantifier("∃", Some(Negation(ForAll("x")(Negation(φ(FunctionParameter(0, 0)))))))
  val Equals = predicate("=", 2, None).copy(attributes = Seq("equality"))
  val ExistsUnique = quantifier("∃!", Some(Exists("y")(ForAll("x")(Equivalence(
    φ(FunctionParameter(0, 0)),
    Equals(FunctionParameter(0, 0), FunctionParameter(0, 1)))))))
  val ElementOf = predicate("∈", 2, None)
  val Subset = predicate("⊆", 2, Some(ForAll("x")(Implication(
      ElementOf(FunctionParameter(0, 0), a),
      ElementOf(FunctionParameter(0, 0), b)))))

  val BlankDefinition = DefinedStatement(Nil, connective("false", 0, None))(Nil)

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
    BlankDefinition,
    None,
    Nil)

  val Pair = TermDefinition(
    "pair",
    Nil,
    Seq(a, b),
    Some("Unordered Pair"),
    Format.Explicit("{%0, %1}", "{a, b}", requiresBrackets = false, requiresComponentBrackets = false),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Product = TermDefinition(
    "product",
    Nil,
    Seq(a, b),
    Some("Cartesian Product"),
    Format.Explicit("%0 × %1", "a × b", requiresBrackets = true, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val First = TermDefinition(
    "first",
    Nil,
    Seq(a),
    None,
    Format.Explicit("%0_0", "a_0", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Second = TermDefinition(
    "second",
    Nil,
    Seq(a),
    None,
    Format.Explicit("%0_1", "a_1", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)

  val ZeroDefinition = TermDefinition(
    "0",
    Nil,
    Nil,
    None,
    Format.default("0", Nil),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Zero = DefinedTerm(Nil, ZeroDefinition)(Nil)
  val NaturalsDefinition = TermDefinition(
    "ℕ",
    Nil,
    Nil,
    None,
    Format.default("ℕ", Nil),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Naturals = DefinedTerm(Nil, NaturalsDefinition)(Nil)
  val Successor = TermDefinition(
    "successor",
    Nil,
    Seq(a),
    None,
    Format.Explicit("a^+", Seq("a"), requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val AdditionDefinition = TermDefinition(
    "+",
    Nil,
    Nil,
    None,
    Format.default("+", Nil),
    Nil,
    BlankDefinition,
    None,
    Nil)
  val Addition = DefinedTerm(Nil, AdditionDefinition)(Nil)
  val Apply = TermDefinition(
    "apply",
    Nil,
    Seq(a, b),
    None,
    Format.Explicit("%0(%1)", "a(b)", requiresBrackets = false, requiresComponentBrackets = true),
    Nil,
    BlankDefinition,
    None,
    Nil)

  val entryContext: EntryContext = EntryContext(
    Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists, ExistsUnique,
      ElementOf, Equals, Subset) ++
      Seq(
        EmptySetDefinition, PowerSet, Singleton, Pair, Product, First, Second,
        ZeroDefinition, NaturalsDefinition, Successor, AdditionDefinition, Apply),
    Nil)

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
  implicit def statementVariableToComponentType(statementVariable: StatementVariable): ComponentType.StatementComponent = ComponentType.StatementComponent(statementVariable.name)
  implicit def termVariableToComponentType(termVariable: TermVariable): ComponentType.TermComponent = ComponentType.TermComponent(termVariable.name)
  implicit def variableTupleToString[T](tuple: (ExpressionVariable[_], T)): (String, T) = tuple.mapLeft(_.name)
  implicit def variableTupleTupleToString[T](tuple: ((ExpressionVariable[_], Int), T)): ((String, Int), T) = tuple.mapLeft(_.mapLeft(_.name))
  implicit def entryContextToParsingContext(implicit entryContext: EntryContext): ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
  implicit def entryContextToProvingContext(implicit entryContext: EntryContext): ProvingContext = ProvingContext(entryContext, new Definitions(entryContext.availableEntries))
  implicit def entryContextAndStepContextToStepProvingContext(implicit entryContext: EntryContext, stepContext: StepContext): StepProvingContext = {
    StepProvingContext(stepContext, entryContextToProvingContext(entryContext))
  }
}
