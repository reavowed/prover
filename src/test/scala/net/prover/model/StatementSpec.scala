package net.prover.model

class StatementSpec extends ProverSpec {

  def parseStatement(line: String): Statement = {
    Statement.parser(defaultContext).parseAndDiscard(line)
  }

  def parseStatementList(line: String): Seq[Statement] = {
    Statement.listParser(defaultContext).parseAndDiscard(line)
  }

  "statement parser" should {
    "parse a statement variable" in {
      parseStatement("φ") mustEqual φ
    }

    "parse a binary connective" in {
      parseStatement("→ φ ψ") mustEqual Implication(φ, ψ)
    }

    "parse a nested binary connective" in {
      parseStatement("→ → φ ψ χ") mustEqual
        Implication(Implication(φ, ψ), χ)
    }

    "parse a quantified statement" in {
      parseStatement("∀ y χ") mustEqual ForAll(y, χ)
    }

    "parse a substitution statement" in {
      parseStatement("sub y x φ") mustEqual SubstitutedStatementVariable(φ, y, x)
    }

    "parse an empty list" in {
      parseStatementList("()") mustEqual Nil
    }

    "parse a list with a single statement" in {
      parseStatementList("(φ)") mustEqual Seq(φ)
    }

    "parse a list with multiple statements" in {
      parseStatementList("(φ, ψ, χ)") mustEqual Seq(φ, ψ, χ)
    }
  }

  "statement match" should {
    "match a statement variable to anything" in {
      φ.calculateSubstitutions(Implication(φ, ψ), PartialSubstitutions.empty) mustEqual
        Some(PartialSubstitutions(
          Map(φ -> Implication(φ, ψ)),
          Map.empty,
          Map.empty))
    }
    "not match a connective to a statement variable" in {
      Implication(φ, ψ).calculateSubstitutions(φ, PartialSubstitutions.empty) must beNone
    }
    "not match two different connectives" in {
      Implication(φ, ψ).calculateSubstitutions(Conjunction(φ, ψ), PartialSubstitutions.empty) must beNone
    }
    "not match two connectives of the same type whose substatements don't match" in {
      Implication(Implication(φ, ψ), χ)
        .calculateSubstitutions(Implication(φ, ψ), PartialSubstitutions.empty)
        .must(beNone)
    }
    "match two connectives of the same type that only differ in statement variable number" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(φ, χ), PartialSubstitutions.empty)
        .mustEqual(Some(PartialSubstitutions(
          Map(φ -> φ, ψ -> χ),
          Map.empty,
          Map.empty)))
    }
    "match two connectives of the same type whose substatements are different but match" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), PartialSubstitutions.empty)
        .mustEqual(Some(PartialSubstitutions(
          Map(
            φ -> Conjunction(φ, ψ),
            ψ -> χ),
          Map.empty,
          Map.empty)))
    }
    "match two connectives of the same type whose substatements merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)), PartialSubstitutions.empty)
        .mustEqual(Some(PartialSubstitutions(
          Map(φ -> Conjunction(φ, ψ)),
          Map.empty,
          Map.empty)))
    }
    "match two connectives of the same type whose substatements do not merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), PartialSubstitutions.empty)
        .must(beNone)
    }
  }

//  "statement term substitution" should {
//    "not do anything if substituting a variable for itself" in {
//      val statements = Seq(
//        φ,
//        Implication(φ, ψ),
//        Equals(x, y))
//
//      forall(statements) { s =>
//        s.substituteFreeVariable(x, x) mustEqual s
//      }
//    }
//    "not do anything if substituting an already-substituted variable" in {
//      val statements = Seq(
//        φ,
//        Implication(φ, ψ),
//        Equals(x, y))
//
//      forall(statements) { s =>
//        val firstSubstitution = s.substituteFreeVariable(y, x)
//        firstSubstitution.substituteFreeVariable(z, x) mustEqual firstSubstitution
//        val secondSubstitution = firstSubstitution.substituteFreeVariable(EmptySet, y)
//        secondSubstitution.substituteFreeVariable(z, x) mustEqual secondSubstitution
//        secondSubstitution.substituteFreeVariable(z, y) mustEqual secondSubstitution
//      }
//    }
//
//    "eliminate extra replacements when simplifying" in {
//      val a = TermVariable("a")
//      val b = TermVariable("b")
//      val p = TermVariable("p")
//      val q = TermVariable("q")
//
//      val firstSubstitution = φ
//        .substituteFreeVariable(y, x)
//        .substituteFreeVariable(b, a)
//        .substituteFreeVariable(q, p)
//      val secondSubstitution = φ
//        .substituteFreeVariable(b, a)
//
//      val distinctVariables = firstSubstitution.attemptSimplification(secondSubstitution)
//      distinctVariables mustEqual Some(DistinctVariables(Map(
//        x -> Variables(Seq(φ), Nil),
//        p -> Variables(Seq(φ), Nil))))
//
//      firstSubstitution.makeSimplifications(distinctVariables.get) mustEqual secondSubstitution
//    }
//
//    "combine replacements when simplifying" in {
//      val firstSubstitution = φ
//        .substituteFreeVariable(y, x)
//        .substituteFreeVariable(z, y)
//      val secondSubstitution = φ
//        .substituteFreeVariable(z, x)
//
//      val distinctVariables = firstSubstitution.attemptSimplification(secondSubstitution)
//      distinctVariables mustEqual Some(DistinctVariables(Map(
//        y -> Variables(Seq(φ), Nil))))
//
//      firstSubstitution.makeSimplifications(distinctVariables.get) mustEqual secondSubstitution
//    }
//
//    "not allow substituting a bound variable" in {
//      ForAll(y, Equals(y, z))
//          .substituteFreeVariable(y, z) must throwAn[Exception]
//    }
//
//    "not allow substituting a bound variable after full substitution" in {
//      ForAll(x, φ)
//        .applySubstitutions(Substitutions(
//          Map(φ -> Equals(y, z)),
//          Map(x -> y)))
//        .substituteFreeVariable(y, z) must throwAn[Exception]
//    }
//  }
}
