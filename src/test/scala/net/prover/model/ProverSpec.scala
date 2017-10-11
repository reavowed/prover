package net.prover.model

import java.nio.file.Paths

import net.prover.model.entries.ExpressionDefinition.{ComponentType, StatementComponent, TermComponent}
import net.prover.model.expressions._
import net.prover.model.entries.{ExpressionDefinition, StatementDefinition, TermDefinition}
import net.prover.model.proof.Fact
import org.specs2.mutable.Specification

trait ProverSpec extends Specification {

  val φ = StatementVariable("φ", 0)
  val ψ = StatementVariable("ψ", 0)
  val χ = StatementVariable("χ", 0)

  val a = TermVariable("a", 0)
  val b = TermVariable("b", 0)
  val c = TermVariable("c", 0)
  val n = TermVariable("n", 0)

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
      symbol,
      Format.default(symbol, componentTypes.map(_.name)),
      definingStatement,
      "",
      "")
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
      symbol,
      Format.default(symbol, componentTypes.map(_.name)),
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
      Seq(ExpressionDefinition.PredicateComponent("φ", Seq(ExpressionDefinition.ComponentArgument("x", 0)))),
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
  val Exists = quantifier("∃", Some(Negation(ForAll("x")(Negation.!(φ.!(FunctionParameter("x", 0)))))))
  val Equals = predicate("=", 2, None)
  val ExistsUnique = quantifier("∃!", Some(Exists("y")(ForAll("x")(Equivalence(
    φ.!(FunctionParameter("x", 0, 1, 2)),
    Equals(FunctionParameter("x", 0, 2, 2), FunctionParameter("y", 0, 1, 2)))))))
  val ElementOf = predicate("∈", 2, None)

  val EmptySetDefinition = TermDefinition(
    "∅",
    Nil,
    Nil,
    "∅",
    Format.default("∅", Nil),
    Nil,
    ForAll.!("x")(Negation.!!(ElementOf.!!(FunctionParameter("x", 0, 2), FunctionParameter.anonymous(0, 1)))),
    "",
    "")
  val EmptySet = DefinedTerm(Nil, EmptySetDefinition, 0)(Nil)

  val PowerSet = TermDefinition(
    "powerSet",
    Nil,
    Seq(ExpressionDefinition.TermComponent("a")),
    "Power Set",
    Format("𝒫%0", requiresBrackets = false),
    Nil,
    ForAll.!("y")(Equivalence.!!(
      ElementOf.!!(FunctionParameter("y", 0, 2), FunctionParameter.anonymous(0, 1, 2)),
      Equals.!!(FunctionParameter("y", 0, 2), StatementVariable("a", 2)))),
    "",
    "")

  implicit val defaultContext = ParsingContext(
    statementDefinitions = Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists, ExistsUnique,
      ElementOf, Equals),
    termDefinitions = Seq(EmptySetDefinition, PowerSet),
    statementVariableNames = Set(φ, ψ, χ).map(_.name),
    termVariableNames = Set(a, b, c, n).map(_.name),
    Seq.empty)

  val stubBook = Book("", Paths.get(""), Nil, Nil, Set.empty, Set.empty)
  val stubChapter = Chapter("", "", "")

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseAndDiscard(text, Paths.get(""))
    }
  }
  implicit class StatementOps(statement: Statement) {
    def ^ : Statement = {
      statement.increaseDepth(1)
    }
  }
  implicit class StatementVariableOps(statementVariable: StatementVariable) {
    def apply(terms: Term*) = PredicateApplication(statementVariable.name, terms, 0)
    def !(terms: Term*) = PredicateApplication(statementVariable.name, terms, 1)
    def !!(terms: Term*) = PredicateApplication(statementVariable.name, terms, 2)
  }
  implicit def statementVariableToComponentType(statementVariable: StatementVariable): StatementComponent = StatementComponent(statementVariable.name)
  implicit def termVariableToComponentType(termVariable: TermVariable): TermComponent = TermComponent(termVariable.name)
  implicit def variableTupleToString[T](tuple: (ExpressionVariable[_], T)): (String, T) = (tuple._1.name, tuple._2)
  implicit class StatementDefinitionOps(statementDefinition: StatementDefinition) {
    def apply(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition, 0)(statementDefinition.boundVariableNames)
    }
    def apply(boundVariableNames: String*)(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition, 0)(boundVariableNames)
    }
    def !(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition, 1)(statementDefinition.boundVariableNames)
    }
    def !(boundVariableNames: String*)(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition, 1)(boundVariableNames)
    }
    def !!(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition, 2)(statementDefinition.boundVariableNames)
    }
    def !!(boundVariableNames: String*)(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition, 2)(boundVariableNames)
    }
    def !!!(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition, 3)(statementDefinition.boundVariableNames)
    }
    def !!!(boundVariableNames: String*)(components: Expression*): DefinedStatement = {
      DefinedStatement(components, statementDefinition, 3)(boundVariableNames)
    }
  }
  implicit class TermOps(term: Term) {
    def ^ : Term = {
      term.increaseDepth(1)
    }
    def ^^ : Term = {
      term.increaseDepth(2)
    }
    def ^^^ : Term = {
      term.increaseDepth(3)
    }
  }
  implicit class TermDefinitionOps(termDefinition: TermDefinition) {
    def apply(components: Expression*): DefinedTerm = {
      DefinedTerm(components, termDefinition, 0)(termDefinition.boundVariableNames)
    }
    def apply(boundVariableNames: String*)(components: Expression*): DefinedTerm = {
      DefinedTerm(components, termDefinition, 0)(boundVariableNames)
    }
    def !(components: Expression*): DefinedTerm = {
      DefinedTerm(components, termDefinition, 1)(termDefinition.boundVariableNames)
    }
    def !(boundVariableNames: String*)(components: Expression*): DefinedTerm = {
      DefinedTerm(components, termDefinition, 1)(boundVariableNames)
    }
    def !!(components: Expression*): DefinedTerm = {
      DefinedTerm(components, termDefinition, 2)(termDefinition.boundVariableNames)
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
