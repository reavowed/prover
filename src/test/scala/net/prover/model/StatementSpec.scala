package net.prover.model

import org.specs2.mutable.Specification

class StatementSpec extends Specification {
  val Implication = Connective("implies", "→", 2)
  val Conjunction = Connective("and", "∧", 2)

  "statement parser" should {
    "parse an atom" in {
      Statement.parse("1", Nil)._1 mustEqual Atom(1)
    }

    "parse a binary connective" in {
      Statement.parse("implies 1 2", Seq(Implication))._1 mustEqual ConnectiveStatement(Seq(Atom(1), Atom(2)), Implication)
    }

    "parse a nested binary connective" in {
      Statement.parse("implies implies 1 2 3", Seq(Implication))._1
        .mustEqual(Implication(Implication(Atom(1), Atom(2)), Atom(3)))
    }
  }

  "statement match" should {
    "match an atom to anything" in {
      Atom(1).attemptMatch(Implication(Atom(1), Atom(2))) mustEqual Some(Map(1 -> Implication(Atom(1), Atom(2))))
    }
    "not match a connective to an atom" in {
      Implication(Atom(1), Atom(2)).attemptMatch(Atom(1)) must beNone
    }
    "not match two different connectives" in {
      Implication(Atom(1), Atom(2)).attemptMatch(Conjunction(Atom(1), Atom(2))) must beNone
    }
    "not match two connectives of the same type whose substatements don't match" in {
      Implication(Implication(Atom(1), Atom(2)), Atom(3))
        .attemptMatch(Implication(Atom(1), Atom(2)))
        .must(beNone)
    }
    "match two connectives of the same type that only differ in atom number" in {
      Implication(Atom(1), Atom(2))
        .attemptMatch(Implication(Atom(1), Atom(3)))
        .mustEqual(Some(Map(1 -> Atom(1), 2 -> Atom(3))))
    }
    "match two connectives of the same type whose substatements are different but match" in {
      Implication(Atom(1), Atom(2))
        .attemptMatch(Implication(Conjunction(Atom(1), Atom(2)), Atom(3)))
        .mustEqual(Some(Map(1 -> Conjunction(Atom(1), Atom(2)), 2 -> Atom(3))))
    }
    "match two connectives of the same type whose substatements merge correctly" in {
      Implication(Atom(1), Atom(1))
        .attemptMatch(Implication(Conjunction(Atom(1), Atom(2)), Conjunction(Atom(1), Atom(2))))
        .mustEqual(Some(Map(1 -> Conjunction(Atom(1), Atom(2)))))
    }
    "match two connectives of the same type whose substatements do not merge correctly" in {
      Implication(Atom(1), Atom(1))
        .attemptMatch(Implication(Conjunction(Atom(1), Atom(2)), Atom(3)))
        .must(beNone)
    }
  }
}
