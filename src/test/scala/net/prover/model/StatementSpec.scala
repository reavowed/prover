package net.prover.model

import net.prover.model.components.{Statement, SubstitutedStatementVariable}

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
      parseStatement("sub y x φ") mustEqual φ.sub(y, x)
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
        DistinctVariables.empty)
      φ.applySubstitutions(substitutions) must beSome(Implication(φ, ψ))
    }

    "apply to a substituted statement variable" in {
      val substitutions = Substitutions(
        Map(φ -> Implication(φ, ψ), ψ -> χ, x -> z, y -> y),
        DistinctVariables.empty)

      φ.sub(y, x).applySubstitutions(substitutions) must
        beSome(Implication(φ.sub(y, z), ψ.sub(y, z)))
    }

    "allow substituting a term into a statement in which a variable of that term is bound" in {
      val substitutions = Substitutions(
        Map(φ -> ForAll(x, Equals(z, OrderedPair(x, y))), x -> z, y -> OrderedPair(x, y)),
        DistinctVariables(x -> z, y -> z))

      φ.sub(y, x).applySubstitutions(substitutions) must
        beSome(ForAll(x, Equals(OrderedPair(x, y), OrderedPair(x, y))))
    }

    "cancel out a single substitution with the right distinct variable condition" in {
      val substitutions = Substitutions(
        Map(φ -> φ.sub(y, x), x -> y, y -> x),
        DistinctVariables(y -> φ))

      φ.sub(y, x).applySubstitutions(substitutions) must beSome(φ)

    }
  }

  "statement match" should {
    "match a statement variable to anything" in {
      φ.calculateSubstitutions(Implication(φ, ψ), PartialSubstitutions.empty) mustEqual
        Seq(PartialSubstitutions(
          Map(φ -> Implication(φ, ψ)),
          Map.empty,
          DistinctVariables.empty))
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
          DistinctVariables.empty)))
    }
    "match two connectives of the same type whose substatements are different but match" in {
      Implication(φ, ψ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), PartialSubstitutions.empty)
        .mustEqual(Seq(PartialSubstitutions(
          Map(
            φ -> Conjunction(φ, ψ),
            ψ -> χ),
          Map.empty,
          DistinctVariables.empty)))
    }
    "match two connectives of the same type whose substatements merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)), PartialSubstitutions.empty)
        .mustEqual(Seq(PartialSubstitutions(
          Map(φ -> Conjunction(φ, ψ)),
          Map.empty,
          DistinctVariables.empty)))
    }
    "match two connectives of the same type whose substatements do not merge correctly" in {
      Implication(φ, φ)
        .calculateSubstitutions(Implication(Conjunction(φ, ψ), χ), PartialSubstitutions.empty)
        .must(beEmpty)
    }

    "allow substitution of a variable for itself in a statement variable" in {
      φ.sub(y, x)
        .calculateSubstitutions(ψ, PartialSubstitutions(Map(φ -> ψ, y -> z, x -> z), Map.empty, DistinctVariables.empty))
        .mustEqual(Seq(PartialSubstitutions(Map(φ -> ψ, y -> z, x -> z), Map.empty, DistinctVariables.empty)))
    }

    "find a distinct variable condition to ensure substitution validity" in {
      φ.sub(y, x)
        .calculateSubstitutions(Equals(z, y), PartialSubstitutions(Map(φ -> Equals(x, y), x -> x), Map.empty, DistinctVariables.empty))
        .mustEqual(Seq(PartialSubstitutions(
          Map(φ -> Equals(x, y), x -> x, y -> z),
          Map.empty,
          DistinctVariables(x -> y))))
    }

    "not add distinct variable conditions for substitution validity if replacing a term with itself" in {
      φ.sub(y, x)
        .calculateSubstitutions(Equals(x, y), PartialSubstitutions(Map(φ -> Equals(x, y), x -> x), Map.empty, DistinctVariables.empty))
        .mustEqual(Seq(PartialSubstitutions(
          Map(φ -> Equals(x, y), x -> x, y -> x),
          Map.empty,
          DistinctVariables.empty)))
    }

    "find a distinct variable condition to ensure substitution validity when required variable is bound" in {
      φ.sub(y, x)
        .calculateSubstitutions(
          ForAll(y, Equals(z, y)),
          PartialSubstitutions(Map(φ -> ForAll(y, Equals(x, y)), x -> x), Map.empty, DistinctVariables.empty))
        .mustEqual(Seq(PartialSubstitutions(
          Map(φ -> ForAll(y, Equals(x, y)), x -> x, y -> z),
          Map.empty,
          DistinctVariables(x -> y))))
    }
  }

  "statement term substitution" should {
    "not do anything if substituting a variable for itself" in {
      val statements = Seq(
        φ,
        Implication(φ, ψ),
        Equals(x, y),
        φ.sub(y, x),
        φ.sub(x, y),
        φ.sub(y, z))

      forall(statements) { s =>
        s.makeSingleSubstitution(x, x, DistinctVariables.empty) must beSome(s)
      }
    }

    "not do anything if substituting an already-substituted variable" in {
      val statements = Seq(
        φ,
        Implication(φ, ψ),
        Equals(x, y))

      forall(statements) { s =>
        val firstSubstitution = s.makeSingleSubstitution(a, z, DistinctVariables.empty).get
        firstSubstitution.makeSingleSubstitution(n, z, DistinctVariables(z -> a)).get mustEqual firstSubstitution
      }
    }

    "not do anything if substituting for a variable that is distinct" in {
      φ
        .makeSingleSubstitution(y, x, DistinctVariables(x -> φ))
        .mustEqual(Some(φ))
    }

    "not do anything if making a double substitution that is known to be distinct from the first" in {
      φ.sub(y, x)
        .makeSingleSubstitution(Y, X, DistinctVariables(X -> φ, y -> X))
        .mustEqual(Some(φ.sub(y, x)))
    }

    "combine substitutions under a distinct condition" in {
      φ.sub(y, x)
        .makeSingleSubstitution(z, y, DistinctVariables(y -> φ))
        .mustEqual(Some(φ.sub(z, x)))
    }

    "not allow substitution inside bound variable without a distinct condition" in {
      ForAll(x, φ).makeSingleSubstitution(z, y, DistinctVariables.empty) must beNone
    }

    "allow substitution inside bound variable without a distinct condition" in {
      ForAll(x, φ).makeSingleSubstitution(z, y, DistinctVariables(y -> x)) must beSome(ForAll(x, φ.sub(z, y)))
    }

    "allow redundant substitution inside bound variable without a distinct condition" in {
      ForAll(x, φ).makeSingleSubstitution(y, y, DistinctVariables.empty) must beSome(ForAll(x, φ))
    }
  }

  "statement finding a substitution" should {
    "find a substitution if the variable has changed appropriately" in {
      Equals(x, y)
        .findSubstitution(Equals(x, z), y)
        .mustEqual((Seq((z, DistinctVariables(y -> x))), None))
    }
    "find no valid substitution if one variable has changed appropriately but the other doesn't match" in {
      Equals(x, y)
        .findSubstitution(Equals(y, z), x)
        .mustEqual((Nil, None))
    }
    "require a distinct variable condition even if every variable either changes or is the target" in {
      Equals(x, y)
        .findSubstitution(Equals(y, y), x)
        .mustEqual((Seq((y, DistinctVariables(x -> y))), None))
    }
    "find a nilpotent substitution" in {
      φ.sub(y, x)
        .findSubstitution(φ.sub(y, x), y)
        .mustEqual((Seq((y, DistinctVariables.empty)), None))
    }
    "find a nilpotent substitution and a distinct variable condition" in {
      φ.sub(y, x)
        .findSubstitution(φ.sub(y, x), z)
        .mustEqual((Seq((z, DistinctVariables.empty)), Some(DistinctVariables(z -> φ) ++ DistinctVariables(z -> y))))
    }
  }

  "statement resolving a substitution" should {
    "allow substitutions into a substituted statement variable with distinct conditions" in {
      φ.sub(y, x)
        .resolveSingleSubstitution(φ.sub(y, x), z, X, Y)
          .mustEqual(Some((φ.sub(y, x), DistinctVariables(z -> φ, z -> y))))
    }
  }

  "statement intersecting variables" should {
    "return substituted variable if the term variable does not match the one being replaced" in {
      φ.sub(y, x)
        .getPotentiallyIntersectingVariables(z) must contain(φ)
    }
    "not return substituted variable if the term variable matches the one being replaced" in {
      φ.sub(y, x)
        .getPotentiallyIntersectingVariables(x) must not contain(φ)
    }
  }

  "statement condensing" should {
    "condense a statement variable with a known substitution to a matching compound statement" in {
      val premise = φ
      val premiseSubstitutions = PartialSubstitutions(
        Map(φ -> Conjunction(φ, ψ)),
        Map.empty,
        DistinctVariables.empty)
      val conclusion = Conjunction(χ, φ)
      val conclusionSubstitutions = PartialSubstitutions.empty
      premise.condense(conclusion, premiseSubstitutions, conclusionSubstitutions) must beSome((
        PartialSubstitutions(Map(φ -> Conjunction(φ, ψ)), Map.empty, DistinctVariables.empty),
        PartialSubstitutions(Map(χ -> φ, φ -> ψ), Map.empty, DistinctVariables.empty)))
    }

    "condense a compound statement to a statement variable with a known matching substitution" in {
      val premise = φ
      val premiseSubstitutions = PartialSubstitutions(
        Map(φ -> Conjunction(φ, ψ)),
        Map.empty,
        DistinctVariables.empty)
      val conclusion = Conjunction(χ, φ)
      val conclusionSubstitutions = PartialSubstitutions.empty
      conclusion.condense(premise, conclusionSubstitutions, premiseSubstitutions) must beSome((
        PartialSubstitutions(Map(χ -> φ, φ -> ψ), Map.empty, DistinctVariables.empty),
        PartialSubstitutions(Map(φ -> Conjunction(φ, ψ)), Map.empty, DistinctVariables.empty)))
    }
  }
}
