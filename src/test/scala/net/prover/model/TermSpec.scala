package net.prover.model

class TermSpec extends ProverSpec {
  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet, Substitutions.empty) mustEqual Seq(Substitutions.empty)
    }

    "match a unary function to another application of the same function" in {
      PowerSet(x)
        .calculateSubstitutions(PowerSet(y), Substitutions.empty)
        .mustEqual(Seq(Substitutions(Map(x -> y), Map.empty)))
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
