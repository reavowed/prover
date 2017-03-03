package net.prover.model

class TermSpec extends ProverSpec {
  val PowerSet = TermSpecification("powerSet", Seq(Term), Format("𝒫x", Seq("x"), requiresBrackets = false))

  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet) must beSome
    }

    "match a unary function to another application of the same function" in {
      PowerSet(Seq(x))
        .calculateSubstitutions(PowerSet(Seq(y)))
        .mustEqual(Some(Substitutions(
          Map.empty,
          Map(x -> y))))
    }
  }

  "term apply match" should {
    "do nothing to a constant" in {
      EmptySet.applySubstitutions(Substitutions(
        Map(φ -> ψ),
        Map(x -> y))
      ) mustEqual EmptySet
    }

    "replace terms in a unary function" in {
      PowerSet(Seq(x)).applySubstitutions(
        Substitutions(
          Map(φ -> ψ),
          Map(x -> y))
      ) mustEqual PowerSet(Seq(y))
    }
  }

}
