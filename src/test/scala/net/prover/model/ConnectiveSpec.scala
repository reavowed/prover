package net.prover.model

import org.specs2.mutable.Specification

class ConnectiveSpec extends Specification {
  val Implication = Connective("implies", "→", 2)
  val Negation = Connective("not", "¬", 1)

  implicit def intToAtom(i: Int): Atom = Atom(i)

  "connective parser" should {
    "parse a binary connective" in {
      Connective.parse("implies → 2")  mustEqual Implication
    }
  }

  "connective" should {
    "parse a binary connective statement from atoms" in {
      Implication.parseStatement("1 2", Nil) mustEqual
        (Implication(1, 2), "")
    }
    "ignore extra text" in {
      Implication.parseStatement("1 2 and 3 4", Nil) mustEqual
        (Implication(1, 2), "and 3 4")
    }
    "throw an exception if not enough statements are supplied" in {
      Implication.parseStatement("1", Nil) must throwAn[Exception]
    }
    "parse nested statements" in {
      Implication.parseStatement("1 not 2", Seq(Negation)) mustEqual
        (Implication(1, Negation(2)), "")
    }
  }
}
