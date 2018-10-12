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

  val stubBook = Book("", Book.Key(""), Nil, Nil, Nil)
  val stubChapter = Chapter("", Chapter.Key("", stubBook.key), "", Nil)

  def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val componentTypes = Seq[ComponentType](φ, ψ, χ).take(size)
    StatementDefinition(
      symbol,
      ChapterEntry.Key.Anchor(symbol, stubChapter.key),
      Nil,
      componentTypes,
      None,
      Format.default(symbol, componentTypes.map(_.name)),
      definingStatement,
      None,
      None)
  }
  def predicate(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    val componentTypes = Seq[ComponentType](a, b, c).take(size)
    StatementDefinition(
      symbol,
      ChapterEntry.Key.Anchor(symbol, stubChapter.key),
      Nil,
      componentTypes,
      None,
      Format.default(symbol, componentTypes.map(_.name)),
      definingStatement,
      None,
      None)
  }

  def quantifier(
    symbol: String,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      ChapterEntry.Key.Anchor(symbol, stubChapter.key),
      Seq("x"),
      Seq(ExpressionDefinition.PredicateComponent("φ", Seq(ExpressionDefinition.ComponentArgument("x", 0)))),
      None,
      Format.Explicit(s"($symbol%0)%1", s"(${symbol}x)φ", requiresBrackets = false),
      definingStatement,
      None,
      None)
  }

  val Implication = connective("→", 2, None).copy(structureType = Some(StatementDefinition.StructureType.Deduction))
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(φ, Negation(ψ)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(φ), ψ)))
  val Equivalence = connective("↔", 2, None)

  val ForAll = quantifier("∀", None).copy(structureType = Some(StatementDefinition.StructureType.Scoping))
  val Exists = quantifier("∃", Some(Negation(ForAll("x")(Negation(φ(FunctionParameter("x", 0, 0)))))))
  val Equals = predicate("=", 2, None)
  val ExistsUnique = quantifier("∃!", Some(Exists("y")(ForAll("x")(Equivalence(
    φ(FunctionParameter("x", 0, 0)),
    Equals(FunctionParameter("x", 0, 0), FunctionParameter("y", 0, 1)))))))
  val ElementOf = predicate("∈", 2, None)
  val Subset = predicate("⊆", 2, Some(ForAll("x")(Implication(
      ElementOf(FunctionParameter("x", 0, 0), a),
      ElementOf(FunctionParameter("x", 0, 0), b)))))

  val EmptySetDefinition = TermDefinition(
    "∅",
    ChapterEntry.Key.Anchor("∅", stubChapter.key),
    Nil,
    Nil,
    None,
    Format.default("∅", Nil),
    Nil,
    ForAll("x")(Negation(ElementOf(FunctionParameter("x", 0, 0), FunctionParameter.anonymous(0, 1)))),
    None)
  val EmptySet = DefinedTerm(Nil, EmptySetDefinition)(Nil)

  val PowerSet = TermDefinition(
    "powerSet",
    ChapterEntry.Key.Anchor("powerSet", stubChapter.key),
    Nil,
    Seq(a),
    Some("Power Set"),
    Format.Explicit("𝒫%0", "𝒫a", requiresBrackets = false),
    Nil,
    ForAll("y")(Equivalence(
      ElementOf(FunctionParameter("y", 0, 0), FunctionParameter.anonymous(0, 1)),
      Subset(FunctionParameter("y", 0, 0), a))),
    None)

  val Singleton = TermDefinition(
    "singleton",
    ChapterEntry.Key.Anchor("singleton", stubChapter.key),
    Nil,
    Seq(a),
    Some("Singleton"),
    Format.Explicit("{%0}", "{a}", requiresBrackets = false),
    Nil,
    φ,
    None)

  val Pair = TermDefinition(
    "pair",
    ChapterEntry.Key.Anchor("pair", stubChapter.key),
    Nil,
    Seq(a, b),
    Some("Unordered Pair"),
    Format.Explicit("{%0, %1}", "{a, b}", requiresBrackets = false),
    Nil,
    φ,
    None)

  implicit val defaultContext = ParsingContext(
    inferences = Nil,
    statementDefinitions = Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists, ExistsUnique,
      ElementOf, Equals, Subset),
    termDefinitions = Seq(EmptySetDefinition, PowerSet, Singleton, Pair),
    termVariableNames = Set.empty,
    Seq.empty)

  implicit class ParserOps[T](parser: Parser[T]) {
    def parseAndDiscard(text: String): T = {
      parser.parseAndDiscard(Tokenizer.fromString(text, "test"))
    }
  }
  implicit class StatementVariableOps(statementVariable: StatementVariable) {
    def apply(terms: Term*) = PredicateApplication(statementVariable.name, terms)
  }
  implicit def statementVariableToComponentType(statementVariable: StatementVariable): StatementComponent = StatementComponent(statementVariable.name)
  implicit def termVariableToComponentType(termVariable: TermVariable): TermComponent = TermComponent(termVariable.name)
  implicit def variableTupleToString[T](tuple: (ExpressionVariable[_], T)): (String, T) = (tuple._1.name, tuple._2)
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

  trait PremiseMagnet {
    def elidable: PremiseMagnet
    def toPremise(index: Int): Premise
  }
  implicit class FromStatement(statement: Statement)(implicit isElidable: Boolean = false) extends PremiseMagnet {
    def elidable: PremiseMagnet = FromStatement(statement)(isElidable = true)
    def toPremise(index: Int) = Premise(statement, index)(isElidable)
  }
  implicit def allToPremise(magnets: Seq[PremiseMagnet]): Seq[Premise] = {
    magnets.mapWithIndex(_.toPremise(_))
  }
}
