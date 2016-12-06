package net.prover.model

import org.specs2.mutable.Specification

trait ProverSpec extends Specification {
  val Implication = Connective("→", 2, None)
  val Negation = Connective("¬", 1, None)
  val Conjunction = Connective("∧", 2, Some(Negation(Implication(1, Negation(2)))))
  val Disjunction = Connective("∨", 2, Some(Implication(Negation(1), 2)))
  val Equivalence = Connective("↔", 2, None)

  val ForAll = Quantifier("∀", None)
  val ElementOf = Predicate("∈", 2, None)
  val Equals = Predicate("=", 2, None)

  val defaultContext = Context(
    connectives = Seq(Implication, Negation, Conjunction, Disjunction, Equivalence),
    quantifiers = Seq(ForAll),
    predicates = Seq(ElementOf, Equals),
    rules = Nil,
    theorems = Nil,
    axioms = Nil,
    termDefinitions = Nil)

  implicit def intToStatementVariable(i: Int): StatementVariable = StatementVariable(i)

  implicit def intToTermVariable(i: Int): TermVariable = TermVariable(i)

  implicit def stringToPartialLine(s: String): PartialLine = PartialLine(s, BookLine(s, 1, "Fake Book"))

  implicit def stringsToLines(strings: Seq[String]): Seq[BookLine] = {
    strings.zipWithIndex.map { case(s, i) => BookLine(s, i+1, "Fake Book")}
  }

  implicit class TheoremBuilderOps(theoremBuilder: TheoremBuilder) {
    def addStep(statement: Statement): TheoremBuilder = theoremBuilder.addStep(Step(statement))
  }
}
