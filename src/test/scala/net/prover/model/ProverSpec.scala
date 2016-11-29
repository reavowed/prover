package net.prover.model

import org.specs2.mutable.Specification

trait ProverSpec extends Specification {
  val Implication = Connective("implies", "→", 2)
  val Negation = Connective("not", "¬", 1)
  val Conjunction = Connective("and", "∧", 2)
  val Disjunction = Connective("or", "∨", 2)

  val defaultContext = Context(
    connectives = Seq(Implication, Negation, Conjunction, Disjunction),
    rules = Nil,
    theorems = Nil,
    definitions = Nil)

  implicit def intToAtom(i: Int): Atom = Atom(i)

  implicit def stringToPartialLine(s: String): PartialLine = PartialLine(s, BookLine(s, 1))

  implicit def stringsToLines(strings: Seq[String]): Seq[BookLine] = {
    strings.zipWithIndex.map((BookLine.apply _).tupled)
  }

  implicit class TheoremBuilderOps(theoremBuilder: TheoremBuilder) {
    def addStep(statement: Statement): TheoremBuilder = theoremBuilder.addStep(Step(statement))
  }
}
