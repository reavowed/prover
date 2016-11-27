package net.prover.model

import org.specs2.matcher.{BeEqualTo, BeTypedEqualTo, Matcher}

class ConnectiveSpec extends ProverSpec {
  "connective parser" should {
    "parse a binary connective" in {
      Connective.parse("implies → 2", Book(""))  mustEqual Implication
    }
  }

  "connective" should {
    def beStatementAndLine(statement: Statement, line: String): Matcher[(Statement, PartialLine)] = {
      beEqualTo((statement, line)) ^^ {
        (_: (Statement, PartialLine)).mapRight(_.remainingText)
      }
    }

    "parse a binary connective statement from atoms" in {
      Implication.parseStatement("1 2", Nil) must
        beStatementAndLine(Implication(1, 2), "")
    }
    "ignore extra text" in {
      Implication.parseStatement("1 2 and 3 4", Nil) must
        beStatementAndLine(Implication(1, 2), "and 3 4")
    }
    "throw an exception if not enough statements are supplied" in {
      Implication.parseStatement("1", Nil) must throwAn[Exception]
    }
    "parse nested statements" in {
      Implication.parseStatement("1 not 2", Seq(Negation)) must
        beStatementAndLine(Implication(1, Negation(2)), "")
    }
  }
}
