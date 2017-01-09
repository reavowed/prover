package net.prover.model

class StatementSpec extends ProverSpec {
  "statement parser" should {
    "parse a statement variable" in {
      Statement.parse("1", defaultContext)._1 mustEqual StatementVariable(1)
    }

    "parse a binary connective" in {
      Statement.parse("→ 1 2", defaultContext)._1 mustEqual ConnectiveStatement(Seq(StatementVariable(1), StatementVariable(2)), Implication)
    }

    "parse a nested binary connective" in {
      Statement.parse("→ → 1 2 3", defaultContext)._1
        .mustEqual(Implication(Implication(StatementVariable(1), StatementVariable(2)), StatementVariable(3)))
    }

    "parse a quantified statement" in {
      Statement.parse("∀ 2 3", defaultContext)._1 mustEqual
        ForAll(2, 3)
    }

    "parse a replacement statement" in {
      Statement.parse("sub 2 1 3", defaultContext)._1 mustEqual StatementVariableWithReplacement(3, 2, 1)
    }
  }

  "statement match" should {
    "match a statement variable to anything" in {
      StatementVariable(1).calculateSubstitutions(Implication(StatementVariable(1), StatementVariable(2))) mustEqual
        Some(Substitutions(
          Map(StatementVariable(1) -> Implication(StatementVariable(1), StatementVariable(2))),
          Map.empty))
    }
    "not match a connective to a statement variable" in {
      Implication(StatementVariable(1), StatementVariable(2)).calculateSubstitutions(StatementVariable(1)) must beNone
    }
    "not match two different connectives" in {
      Implication(StatementVariable(1), StatementVariable(2)).calculateSubstitutions(Conjunction(StatementVariable(1), StatementVariable(2))) must beNone
    }
    "not match two connectives of the same type whose substatements don't match" in {
      Implication(Implication(StatementVariable(1), StatementVariable(2)), StatementVariable(3))
        .calculateSubstitutions(Implication(StatementVariable(1), StatementVariable(2)))
        .must(beNone)
    }
    "match two connectives of the same type that only differ in statement variable number" in {
      Implication(StatementVariable(1), StatementVariable(2))
        .calculateSubstitutions(Implication(StatementVariable(1), StatementVariable(3)))
        .mustEqual(Some(Substitutions(
          Map(StatementVariable(1) -> StatementVariable(1), StatementVariable(2) -> StatementVariable(3)),
          Map.empty)))
    }
    "match two connectives of the same type whose substatements are different but match" in {
      Implication(StatementVariable(1), StatementVariable(2))
        .calculateSubstitutions(Implication(Conjunction(StatementVariable(1), StatementVariable(2)), StatementVariable(3)))
        .mustEqual(Some(Substitutions(
          Map(
            StatementVariable(1) -> Conjunction(1, 2),
            StatementVariable(2) -> StatementVariable(3)),
          Map.empty)))
    }
    "match two connectives of the same type whose substatements merge correctly" in {
      Implication(StatementVariable(1), StatementVariable(1))
        .calculateSubstitutions(Implication(Conjunction(StatementVariable(1), StatementVariable(2)), Conjunction(StatementVariable(1), StatementVariable(2))))
        .mustEqual(Some(Substitutions(
          Map(StatementVariable(1) -> Conjunction(1, 2)),
          Map.empty)))
    }
    "match two connectives of the same type whose substatements do not merge correctly" in {
      Implication(StatementVariable(1), StatementVariable(1))
        .calculateSubstitutions(Implication(Conjunction(StatementVariable(1), StatementVariable(2)), StatementVariable(3)))
        .must(beNone)
    }
  }

  "statement term substitution" should {
    "not do anything if substituting a variable for itself" in {
      val statements = Seq(
        StatementVariable(1),
        Implication(1, 2),
        Equals(1, 2))

      forall(statements) { s =>
        s.substituteFreeVariable(1, 1) mustEqual s
      }
    }
    "not do anything if substituting an already-substituted variable" in {
      val statements = Seq(
        StatementVariable(1),
        Implication(1, 2),
        Equals(1, 2))

      forall(statements) { s =>
        val firstSubstitution = s.substituteFreeVariable(2, 1)
        firstSubstitution.substituteFreeVariable(3, 1) mustEqual firstSubstitution
      }
    }
  }
}
