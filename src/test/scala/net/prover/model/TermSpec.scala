package net.prover.model

import net.prover.model.components.BoundVariable

class TermSpec extends ProverSpec {
  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet, Substitutions.empty, 0) mustEqual Seq(Substitutions.empty)
    }

    "match a unary function to another application of the same function" in {
      PowerSet(x)
        .calculateSubstitutions(PowerSet(y), Substitutions.empty, 0)
        .mustEqual(Seq(Substitutions(Map(x -> y), Map.empty)))
    }

    "match a term variable to a bound variable outside the current statement" in {
      x.calculateSubstitutions(BoundVariable(2)("y"), Substitutions.empty, 2)
          .mustEqual(Seq(Substitutions(Map(x -> BoundVariable(2)("y")), Map.empty)))
    }

    "not match a term variable to a bound variable within the current statement" in {
      x.calculateSubstitutions(BoundVariable(2)("y"), Substitutions.empty, 3)
        .mustEqual(Nil)
    }
  }

  "term apply match" should {
    "do nothing to a constant" in {
      EmptySet.applySubstitutions(Substitutions(Map(φ -> ψ, x -> y), Map.empty)) must beSome(EmptySet)
    }

    "replace terms in a unary function" in {
      PowerSet(x).applySubstitutions(Substitutions(Map(φ -> ψ, x -> y), Map.empty)) must beSome(PowerSet(y))
    }
  }

}
