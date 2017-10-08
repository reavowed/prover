package net.prover.model

import java.nio.file.Paths

import net.prover.model.expressions._
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
      Format.default(symbol, variables.map(_.name)),
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
      Format.default(symbol, variables.map(_.name)),
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
      Seq(PredicateApplicationVariable(φ.!, Seq(FunctionParameter("x", 0)), 1)),
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
    ForAll("x")(Negation.!(ElementOf.!(FunctionParameter("x", 0), ConstantFunction(PlaceholderTerm, 0)))),
    "",
    "")
  val EmptySet = EmptySetDefinition()

  val PowerSet = TermDefinition(
    "powerSet",
    Seq(x),
    "Power Set",
    Format("𝒫%0", requiresBrackets = false),
    Nil,
    ForAll("y")(Equivalence.!(
      ElementOf.!(FunctionParameter("y", 0), ConstantFunction(PlaceholderTerm, 0)),
      Equals.!(FunctionParameter("y", 0), ConstantFunction(x, 0)))),
    "",
    "")

  implicit val defaultContext = ParsingContext(
    statementDefinitions = Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists, ExistsUnique,
      ElementOf, Equals),
    termDefinitions = Seq(EmptySetDefinition, PowerSet),
    statementVariableNames = Set(φ, ψ, χ).map(_.name),
    termVariableNames = Set(x, y, z, X, Y, Z, a, b, n).map(_.name),
    Seq.empty)

  val stubBook = Book("", Paths.get(""), Nil, Nil, Set.empty, Set.empty)
  val stubChapter = Chapter("", "", "")

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseAndDiscard(text, Paths.get(""))
    }
  }
  implicit class StatementVariableOps(statementVariable: StatementVariable) {
    def apply(terms: Term*) = PredicateApplication(this.!, terms)
    def ! : PredicateVariable = PredicateVariable(statementVariable.name)
    def !(functions: Function*): MetaPredicateApplication = MetaPredicateApplication(this.!, functions, 1)

  }
  implicit class StatementDefinitionOps(statementDefinition: StatementDefinition) {
    def apply(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition)(statementDefinition.boundVariableNames)
    }
    def apply(boundVariableNames: String*)(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition)(boundVariableNames)
    }
    def !(components: ExpressionFunction[Expression]*): DefinedPredicate = {
      DefinedPredicate(components, statementDefinition, 1)(statementDefinition.boundVariableNames)
    }
    def !(boundVariableNames: String*)(components: ExpressionFunction[Expression]*): DefinedPredicate = {
      DefinedPredicate(components, statementDefinition, 1)(boundVariableNames)
    }
    def !!(components: ExpressionFunction[Expression]*): DefinedPredicate = {
      DefinedPredicate(components, statementDefinition, 2)(statementDefinition.boundVariableNames)
    }
  }
  implicit class DefinedTermOps(definedTerm: DefinedTerm) {
    def ! : DefinedFunction = {
      definedTerm.increaseDepth(1)
    }
  }
  implicit class TermDefinitionOps(termDefinition: TermDefinition) {
    def apply(components: Expression*): DefinedTerm = {
      DefinedTerm(components, termDefinition)
    }
    def !(components: ExpressionFunction[Expression]*): DefinedFunction = {
      DefinedFunction(components, termDefinition, 1)
    }
    def !!(components: ExpressionFunction[Expression]*): DefinedFunction = {
      DefinedFunction(components, termDefinition, 2)
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
}
