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

  "statement general substitution" should {
    "apply to a statement variable" in {
      val substitutions = Substitutions(
        Map(φ -> Implication(φ, ψ), ψ -> χ),
        Map.empty)
      φ.applySubstitutions(substitutions, Map.empty) mustEqual Some(Implication(φ, ψ))
    }

    "apply to a substituted statement variable" in {
      val substitutions = Substitutions(
        Map(φ -> Implication(φ, ψ), ψ -> χ),
        Map(x -> z, y -> y))

      SubstitutedStatementVariable(φ, y, x).applySubstitutions(substitutions, Map.empty) mustEqual
        Some(Implication(SubstitutedStatementVariable(φ, y, z), SubstitutedStatementVariable(ψ, y, z)))
    }
  }

  "statement match" should {
    "match a statement variable to anything" in {
      φ.calculateSubstitutions(Implication(φ, ψ), PartialSubstitutions.empty) mustEqual
        Seq(PartialSubstitutions(
          Map(φ -> Implication(φ, ψ)),
          Map.empty,
          Map.empty,
          Map.empty))
    }
    "not match a connective to a statement variable" in {
      Implication(φ, ψ).calculateSubstitutions(φ, PartialSubstitutions.empty) must beEmpty
    }
    "not match two different connectives" in {
      Implication(φ, ψ).calculateSubstitutions(Conjunction(φ, ψ), PartialSubstitutions.empty) must beEmpty
    }
    "not match two connectives of the same type whose substatements don't match" in {
      Implication(Implication(φ, ψ), χ)
        .calculateSubstitutions(Implication(φ, ψ), PartialSubstitutions.empty)
        .must(beEmpty)
    }
    "match two connectives of the same type that only differ in statement variable number" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(φ, χ), PartialSubstitutions.empty)
        .mustEqual(Seq(PartialSubstitutions(
          Map(φ -> φ, ψ -> χ),
          Map.empty,
          Map.empty,
          Map.empty)))
    }
    "match two connectives of the same type whose substatements are different but match" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), PartialSubstitutions.empty)
        .mustEqual(Seq(PartialSubstitutions(
          Map(
            φ -> Conjunction(φ, ψ),
            ψ -> χ),
          Map.empty,
          Map.empty,
          Map.empty)))
    }
    "match two connectives of the same type whose substatements merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)), PartialSubstitutions.empty)
        .mustEqual(Seq(PartialSubstitutions(
          Map(φ -> Conjunction(φ, ψ)),
          Map.empty,
          Map.empty,
          Map.empty)))
    }
    "match two connectives of the same type whose substatements do not merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), PartialSubstitutions.empty)
        .must(beEmpty)
    }
  }

  "statement term substitution" should {
    "not do anything if substituting a variable for itself" in {
      val statements = Seq(
        φ,
        Implication(φ, ψ),
        Equals(x, y))

      forall(statements) { s =>
        s.makeSingleSubstitution(x, x, Map.empty).get mustEqual s
      }
    }
    "not do anything if substituting an already-substituted variable" in {
      val statements = Seq(
        φ,
        Implication(φ, ψ),
        Equals(x, y))

      forall(statements) { s =>
        val firstSubstitution = s.makeSingleSubstitution(y, x, Map.empty).get
        firstSubstitution.makeSingleSubstitution(z, x, Map.empty).get mustEqual firstSubstitution
      }
    }

    "not do anything if substituting for a variable that is distinct" in {
      φ
        .makeSingleSubstitution(y, x, Map(x -> Variables(Set(φ), Set.empty)))
        .mustEqual(Some(φ))
    }

    "not do anything if making a double substitution that is known to be distinct from the first" in {
      SubstitutedStatementVariable(φ, y, x)
        .makeSingleSubstitution(Y, X, Map(X -> Variables(Set(φ), Set.empty), y -> Variables(Set.empty, Set(X))))
        .mustEqual(Some(SubstitutedStatementVariable(φ, y, x)))
    }

    "combine substitutions under a distinct condition" in {
      SubstitutedStatementVariable(φ, y, x)
        .makeSingleSubstitution(z, y, Map(y -> Variables(Set(φ), Set.empty)))
        .mustEqual(Some(SubstitutedStatementVariable(φ, z, x)))
    }
  }

  "statement finding a substitution" should {
    "find no substitution required if target statement is the same and variable is not present" in {
      Equals(x, y)
        .findSubstitution(Equals(x, y), z)
        .mustEqual(Seq((None, Map.empty)))
    }
    "find a substitution if the variable has changed appropriately" in {
      Equals(x, y)
        .findSubstitution(Equals(x, z), y)
        .mustEqual(Seq((Some(z), Map.empty)))
    }
    "find no valid substitution if one variable has changed appropriately but the other doesn't match" in {
      Equals(x, y)
        .findSubstitution(Equals(y, z), x)
        .mustEqual(Nil)
    }
  }
}
