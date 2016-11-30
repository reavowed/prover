package net.prover.model

class StatementSpec extends ProverSpec {
  "statement parser" should {
    "parse a statement variable" in {
      Statement.parse("1", defaultContext)._1 mustEqual StatementVariable(1)
    }

    "parse a binary connective" in {
      Statement.parse("implies 1 2", defaultContext)._1 mustEqual ConnectiveStatement(Seq(StatementVariable(1), StatementVariable(2)), Implication)
    }

    "parse a nested binary connective" in {
      Statement.parse("implies implies 1 2 3", defaultContext)._1
        .mustEqual(Implication(Implication(StatementVariable(1), StatementVariable(2)), StatementVariable(3)))
    }

    "parse a quantified statement" in {
      Statement.parse("∀ 2 3", defaultContext)._1 mustEqual
        ForAll(2, 3)
    }
  }

  "statement match" should {
    "match a statement variable to anything" in {
      StatementVariable(1).attemptMatch(Implication(StatementVariable(1), StatementVariable(2))) mustEqual Some(Map(1 -> Implication(StatementVariable(1), StatementVariable(2))))
    }
    "not match a connective to a statement variable" in {
      Implication(StatementVariable(1), StatementVariable(2)).attemptMatch(StatementVariable(1)) must beNone
    }
    "not match two different connectives" in {
      Implication(StatementVariable(1), StatementVariable(2)).attemptMatch(Conjunction(StatementVariable(1), StatementVariable(2))) must beNone
    }
    "not match two connectives of the same type whose substatements don't match" in {
      Implication(Implication(StatementVariable(1), StatementVariable(2)), StatementVariable(3))
        .attemptMatch(Implication(StatementVariable(1), StatementVariable(2)))
        .must(beNone)
    }
    "match two connectives of the same type that only differ in statement variable number" in {
      Implication(StatementVariable(1), StatementVariable(2))
        .attemptMatch(Implication(StatementVariable(1), StatementVariable(3)))
        .mustEqual(Some(Map(1 -> StatementVariable(1), 2 -> StatementVariable(3))))
    }
    "match two connectives of the same type whose substatements are different but match" in {
      Implication(StatementVariable(1), StatementVariable(2))
        .attemptMatch(Implication(Conjunction(StatementVariable(1), StatementVariable(2)), StatementVariable(3)))
        .mustEqual(Some(Map(1 -> Conjunction(StatementVariable(1), StatementVariable(2)), 2 -> StatementVariable(3))))
    }
    "match two connectives of the same type whose substatements merge correctly" in {
      Implication(StatementVariable(1), StatementVariable(1))
        .attemptMatch(Implication(Conjunction(StatementVariable(1), StatementVariable(2)), Conjunction(StatementVariable(1), StatementVariable(2))))
        .mustEqual(Some(Map(1 -> Conjunction(StatementVariable(1), StatementVariable(2)))))
    }
    "match two connectives of the same type whose substatements do not merge correctly" in {
      Implication(StatementVariable(1), StatementVariable(1))
        .attemptMatch(Implication(Conjunction(StatementVariable(1), StatementVariable(2)), StatementVariable(3)))
        .must(beNone)
    }
  }
}
