package net.prover.model

import org.specs2.mutable.Specification
import shapeless.HNil

trait ProverSpec extends Specification {
  val Implication = Connective("→", 2, None)
  val Negation = Connective("¬", 1, None)
  val Conjunction = Connective("∧", 2, Some(Negation(Implication(1, Negation(2)))))
  val Disjunction = Connective("∨", 2, Some(Implication(Negation(1), 2)))
  val Equivalence = Connective("↔", 2, None)

  val ForAll = Quantifier("∀", None, DistinctVariables.empty)
  var Exists = Quantifier("∃", Some(Negation(ForAll(1, Negation(1)))), DistinctVariables.empty)
  val ElementOf = Predicate("∈", 2, None)
  val Equals = Predicate("=", 2, None)

  val EmptySetSpecification = TermSpecification(
    "∅",
    ComponentTypeList.empty,
    "∅")
  val EmptySetDefinition = TermDefinition(
    EmptySetSpecification,
    ForAll(1, Negation(ElementOf(1, DefinedTerm(HNil, EmptySetSpecification)))))
  val EmptySet = EmptySetSpecification(HNil)

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
    Seq(StatementVariableWithReplacement(1, 2, 1)),
    ForAll(1, 1),
    Seq(2),
    DistinctVariables(Map(TermVariable(2) -> Variables(Seq(1), Nil))))
  val EliminateForall = DirectRule(
    "eliminateForall",
    Seq(ForAll(1, 1)),
    StatementVariableWithReplacement(1, 2, 1),
    Nil,
    DistinctVariables.empty)

  val statementDefinitions = Seq(
    Implication, Negation, Conjunction, Disjunction, Equivalence,
    ForAll, Exists,
    ElementOf, Equals)

  val defaultContext = Context(
    statementDefinitions = Seq(
      Implication, Negation, Conjunction, Disjunction, Equivalence,
      ForAll, Exists,
      ElementOf, Equals),
    termParsers = Nil,
    otherTheoremLineParsers = Nil)

  implicit def intToStatementVariable(i: Int): StatementVariable = StatementVariable(i)

  implicit def intToTermVariable(i: Int): TermVariable = TermVariable(i)

  implicit def stringToPartialLine(s: String): PartialLine = PartialLine(s, BookLine(s, 1, "Fake Book"))

  implicit def stringsToLines(strings: Seq[String]): Seq[BookLine] = {
    strings.zipWithIndex.map { case(s, i) => BookLine(s, i+1, "Fake Book")}
  }

  implicit class TheoremBuilderOps(theoremBuilder: TheoremBuilder) {
    def addStep(statement: Statement): TheoremBuilder = theoremBuilder.addStep(Step(statement, ""))
    def addStep(i: Int): TheoremBuilder = addStep(intToStatementVariable(i))
  }
}
