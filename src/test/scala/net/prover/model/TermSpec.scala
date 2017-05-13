package net.prover.model

class TermSpec extends ProverSpec {
  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet, PartialSubstitutions.empty) mustEqual Seq(PartialSubstitutions.empty)
    }

    "match a unary function to another application of the same function" in {
      PowerSet(x)
        .calculateSubstitutions(PowerSet(y), PartialSubstitutions.empty)
        .mustEqual(Seq(PartialSubstitutions(
          Map.empty,
          Map(x -> y),
          Map.empty,
          DistinctVariables.empty)))
    }
  }

  "term apply match" should {
    "do nothing to a constant" in {
      EmptySet.applySubstitutions(
        Substitutions(
          Map(φ -> ψ),
          Map(x -> y),
          DistinctVariables.empty)
      ) must beSome(EmptySet)
    }

    "replace terms in a unary function" in {
      PowerSet(x).applySubstitutions(
        Substitutions(
          Map(φ -> ψ),
          Map(x -> y),
          DistinctVariables.empty)
      ) must beSome(PowerSet(y))
    }
  }

}
