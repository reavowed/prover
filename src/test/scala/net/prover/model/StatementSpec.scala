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
        ForAll("y", 3)
    }

    "parse a replacement statement" in {
      Statement.parse("sub 2 1 3", defaultContext)._1 mustEqual StatementVariableWithReplacement(3, "y", "z")
    }

    "parse an empty list" in {
      Parser.listInParens("()", Statement.parse(_, defaultContext))._1 mustEqual Nil
    }

    "parse a list with a single statement" in {
      Parser.listInParens("(1)", Statement.parse(_, defaultContext))._1 mustEqual Seq(StatementVariable(1))
    }

    "parse a list with multiple statements" in {
      Parser.listInParens("(1, 2, 3)", Statement.parse(_, defaultContext))._1 mustEqual Seq(
        StatementVariable(1),
        StatementVariable(2),
        StatementVariable(3))
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
        Equals("z", "y"))

      forall(statements) { s =>
        s.substituteFreeVariable("z", "z") mustEqual s
      }
    }
    "not do anything if substituting an already-substituted variable" in {
      val statements = Seq(
        StatementVariable(1),
        Implication(1, 2),
        Equals("z", "y"))

      forall(statements) { s =>
        val firstSubstitution = s.substituteFreeVariable("y", "z")
        firstSubstitution.substituteFreeVariable("x", "z") mustEqual firstSubstitution
        val secondSubstitution = firstSubstitution.substituteFreeVariable(EmptySet, "y")
        secondSubstitution.substituteFreeVariable("x", "z") mustEqual secondSubstitution
        secondSubstitution.substituteFreeVariable("x", "y") mustEqual secondSubstitution
      }
    }

    "eliminate extra replacements when simplifying" in {
      val firstSubstitution = StatementVariable(1)
        .substituteFreeVariable("y", "z")
        .substituteFreeVariable("w", "x")
        .substituteFreeVariable("u", "v")
      val secondSubstitution = StatementVariable(1)
        .substituteFreeVariable("w", "x")

      val distinctVariables = firstSubstitution.attemptSimplification(secondSubstitution)
      distinctVariables mustEqual Some(DistinctVariables(Map(
        TermVariable("z") -> Variables(Seq(StatementVariable(1)), Nil),
        TermVariable("v") -> Variables(Seq(StatementVariable(1)), Nil))))

      firstSubstitution.makeSimplifications(distinctVariables.get) mustEqual secondSubstitution
    }

    "combine replacements when simplifying" in {
      val firstSubstitution = StatementVariable(1)
        .substituteFreeVariable("y", "z")
        .substituteFreeVariable("x", "y")
      val secondSubstitution = StatementVariable(1)
        .substituteFreeVariable("x", "z")

      val distinctVariables = firstSubstitution.attemptSimplification(secondSubstitution)
      distinctVariables mustEqual Some(DistinctVariables(Map(
        TermVariable("y") -> Variables(Seq(StatementVariable(1)), Nil))))

      firstSubstitution.makeSimplifications(distinctVariables.get) mustEqual secondSubstitution
    }
  }
}
