package net.prover.model

import org.specs2.mutable.Specification

trait ProverSpec extends Specification {
  def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq.fill(size)(Statement),
      Format.default(symbol, size),
      (1 to size).map(StatementVariable),
      Nil,
      DistinctVariables.empty,
      definingStatement)
  }
  def predicate(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq.fill(size)(Term),
      Format.default(symbol, size),
      (1 to size).map(123 - _).map(_.toChar.toString).map(TermVariable),
      Nil,
      DistinctVariables.empty,
      definingStatement)
  }

  def quantifier(
    symbol: String,
    definingStatement: Option[Statement],
    distinctVariables: DistinctVariables
  ): StatementDefinition = {
    StatementDefinition(
      symbol,
      Seq(Term, Statement),
      Format(s"($symbol{}){}", false),
      Seq(TermVariable("z"), StatementVariable(1)),
      Seq(TermVariable("z")),
      DistinctVariables.empty,
      definingStatement)
  }


  val Implication = connective("→", 2, None)
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(1, Negation(2)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(1), 2)))
  val Equivalence = connective("↔", 2, None)

  val ForAll = quantifier("∀", None, DistinctVariables.empty)
  var Exists = quantifier("∃", Some(Negation(ForAll("z", Negation(1)))), DistinctVariables.empty)
  val ElementOf = predicate("∈", 2, None)
  val Equals = predicate("=", 2, None)

  val EmptySetSpecification = TermSpecification("∅", Nil, Format.default("∅", 0))
  val EmptySet = DefinedTerm(Nil, EmptySetSpecification)
  val EmptySetDefinition = TermDefinition(
    EmptySetSpecification,
    Nil,
    Nil,
    ForAll("z", Negation(ElementOf("z", EmptySet))))

  val Restate = Axiom(
    "restate",
    "Restate",
    None,
    Seq(StatementVariable(1)),
    StatementVariable(1),
    Nil,
    DistinctVariables.empty)
  val IntroduceImplication = Axiom(
    "introduceImplication",
    "Introduce Implication",
    Some(StatementVariable(1)),
    Seq(StatementVariable(2)),
    Implication(1, 2),
    Nil,
    DistinctVariables.empty)
  val EliminateImplication = Axiom(
    "eliminateImplication",
    "Eliminate Implication",
    None,
    Seq(Implication(1, 2), 1),
    2,
    Nil,
    DistinctVariables.empty)
  val IntroduceForall = Axiom(
    "introduceForall",
    "Introduce Forall",
    None,
    Seq(StatementVariableWithReplacement(1, "y", "z")),
    ForAll("z", 1),
    Seq("y"),
    DistinctVariables(Map(TermVariable("y") -> Variables(Seq(1), Nil))))
  val EliminateForall = Axiom(
    "eliminateForall",
    "Eliminate Forall",
    None,
    Seq(ForAll("z", 1)),
    StatementVariableWithReplacement(1, "y", "z"),
    Nil,
    DistinctVariables.empty)

  val statementDefinitions = Seq(
    Implication, Negation, Conjunction, Disjunction, Equivalence,
    ForAll, Exists,
    ElementOf, Equals)

  val defaultContext = statementDefinitions.foldLeft(Context.empty) { case (context, statementDefinition) =>
    context.addStatementDefinition(statementDefinition)
  }.addTermDefinition(EmptySetDefinition)

  implicit def intToStatementVariable(i: Int): StatementVariable = StatementVariable(i)

  implicit def stringToTermVariable(s: String): TermVariable = TermVariable(s)

  implicit def stringToPartialLine(s: String): PartialLine = PartialLine(s, BookLine(s, 1, "Fake Book", "filename"))

  implicit def stringsToLines(strings: Seq[String]): Seq[BookLine] = {
    strings.zipWithIndex.map { case(s, i) => BookLine(s, i+1, "Fake Book", "filename")}
  }

  implicit class TheoremBuilderOps(theoremBuilder: TheoremBuilder) {
    def addStep(statement: Statement): TheoremBuilder = theoremBuilder.addStep(Step(statement, ""))
    def addStep(i: Int): TheoremBuilder = addStep(intToStatementVariable(i))
  }
}
