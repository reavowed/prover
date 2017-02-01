package net.prover.model

import org.specs2.mutable.Specification
import shapeless.HNil

trait ProverSpec extends Specification {
  def connective(
    symbol: String,
    size: Int,
    definingStatement: Option[Statement]
  ): CustomStatementDefinition = {
    CustomStatementDefinition(
      StatementSpecification(symbol, Seq.fill(size)(Statement), Format.default(symbol, size)),
      (1 to size).map(StatementVariable),
      definingStatement)
  }


  val Implication = connective("→", 2, None)
  val Negation = connective("¬", 1, None)
  val Conjunction = connective("∧", 2, Some(Negation(Implication(1, Negation(2)))))
  val Disjunction = connective("∨", 2, Some(Implication(Negation(1), 2)))
  val Equivalence = connective("↔", 2, None)

  val ForAll = Quantifier("∀", None, DistinctVariables.empty)
  var Exists = Quantifier("∃", Some(Negation(ForAll("z", Negation(1)))), DistinctVariables.empty)
  val ElementOf = Predicate("∈", 2, None)
  val Equals = Predicate("=", 2, None)

  val EmptySetSpecification = TermSpecification("∅", Nil, Format.default("∅", 0))
  val EmptySet = DefinedTerm(Nil, EmptySetSpecification)
  val EmptySetDefinition = TermDefinition(
    EmptySetSpecification,
    Nil,
    Nil,
    ForAll("z", Negation(ElementOf("z", EmptySet))))

  val Restate = DirectRule(
    "restate",
    Seq(StatementVariable(1)),
    StatementVariable(1),
    Nil,
    DistinctVariables.empty)
  val IntroduceImplication = FantasyRule(
    "introduceImplication",
    StatementVariable(1),
    Seq(StatementVariable(2)),
    Implication(1, 2))
  val EliminateImplication = DirectRule(
    "eliminateImplication",
    Seq(Implication(1, 2), 1),
    2,
    Nil,
    DistinctVariables.empty)
  val IntroduceForall = DirectRule(
    "introduceForall",
    Seq(StatementVariableWithReplacement(1, "y", "z")),
    ForAll("z", 1),
    Seq("y"),
    DistinctVariables(Map(TermVariable("y") -> Variables(Seq(1), Nil))))
  val EliminateForall = DirectRule(
    "eliminateForall",
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
