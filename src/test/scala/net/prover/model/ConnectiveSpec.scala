package net.prover.model

import org.specs2.matcher.Matcher

class ConnectiveSpec extends ProverSpec {
  "connective parser" should {
    "parse a connective with no definition" in {
      Connective.parse("→ 2", Context.empty)  mustEqual Connective("→", 2, None)
    }

    "parse a connective with a definition" in {
      Connective.parse(
        "∧ 2 ¬ → 1 ¬ 2",
        Context.empty.copy(connectives = Seq(Implication, Negation))
      ) mustEqual Connective("∧", 2, Some(Negation(Implication(1, Negation(2)))))
    }
  }

  "connective" should {
    def beStatementAndLine(statement: Statement, line: String): Matcher[(Statement, PartialLine)] = {
      beEqualTo((statement, line)) ^^ {
        (_: (Statement, PartialLine)).mapRight(_.remainingText)
      }
    }

    "parse a binary connective statement from statement variables" in {
      Implication.parseStatement("1 2", defaultContext) must
        beStatementAndLine(Implication(1, 2), "")
    }
    "ignore extra text" in {
      Implication.parseStatement("1 2 ∧ 3 4", defaultContext) must
        beStatementAndLine(Implication(1, 2), "∧ 3 4")
    }
    "throw an exception if not enough statements are supplied" in {
      Implication.parseStatement("1", defaultContext) must throwAn[Exception]
    }
    "parse nested statements" in {
      Implication.parseStatement("1 ¬ 2", defaultContext) must
        beStatementAndLine(Implication(1, Negation(2)), "")
    }

    "apply definition to a theorem with the connective" in {
      val theorem = TheoremBuilder().addStep(Conjunction(Implication(1, 2), 3))
      val updatedTheorem = Conjunction.definition.get.readAndUpdateTheoremBuilder(theorem, "1", defaultContext)
      updatedTheorem.steps(1).statement mustEqual Negation(Implication(Implication(1, 2), Negation(3)))
    }
    "apply definition to a theorem with the defining statement" in {
      val theorem = TheoremBuilder().addStep(Negation(Implication(Implication(1, 2), Negation(3))))
      val updatedTheorem = Conjunction.definition.get.readAndUpdateTheoremBuilder(theorem, "1", defaultContext)
      updatedTheorem.steps(1).statement mustEqual Conjunction(Implication(1, 2), 3)
    }
  }
}
