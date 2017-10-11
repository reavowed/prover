package net.prover.model

class TermSpec extends ProverSpec {
  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet, Substitutions.empty) mustEqual Seq(Substitutions.empty)
    }

    "match a unary function to another application of the same function" in {
      PowerSet(a)
        .calculateSubstitutions(PowerSet(b), Substitutions.empty)
        .mustEqual(Seq(Substitutions(Map.empty, Map(a -> b), Map.empty, Map.empty)))
    }
  }

  "term apply match" should {
    "do nothing to a constant" in {
      EmptySet.applySubstitutions(Substitutions(Map(φ -> ψ), Map(a -> b), Map.empty, Map.empty)) must beSome(EmptySet)
    }

    "replace terms in a unary function" in {
      PowerSet(a).applySubstitutions(Substitutions(Map(φ -> ψ), Map(a -> b), Map.empty, Map.empty)) must beSome(PowerSet(b))
    }
  }

}
