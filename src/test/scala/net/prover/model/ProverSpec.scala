package net.prover.model

import org.specs2.mutable.Specification

trait ProverSpec extends Specification {
  val Implication = Connective("→", 2, None)
  val Negation = Connective("¬", 1, None)
  val Conjunction = Connective("∧", 2, Some(Negation(Implication(1, Negation(2)))))
  val Disjunction = Connective("∨", 2, Some(Implication(Negation(1), 2)))

  val ForAll = Quantifier("∀")

  val defaultContext = Context(
    connectives = Seq(Implication, Negation, Conjunction, Disjunction),
    quantifiers = Seq(ForAll),
    predicates = Nil,
    rules = Nil,
    theorems = Nil)

  implicit def intToStatementVariable(i: Int): StatementVariable = StatementVariable(i)

  implicit def intToTermVariable(i: Int): TermVariable = TermVariable(i)

  implicit def stringToPartialLine(s: String): PartialLine = PartialLine(s, BookLine(s, 1))

  implicit def stringsToLines(strings: Seq[String]): Seq[BookLine] = {
    strings.zipWithIndex.map((BookLine.apply _).tupled)
  }

  implicit class TheoremBuilderOps(theoremBuilder: TheoremBuilder) {
    def addStep(statement: Statement): TheoremBuilder = theoremBuilder.addStep(Step(statement))
  }
}
