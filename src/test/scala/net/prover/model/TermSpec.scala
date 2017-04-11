package net.prover.model

class TermSpec extends ProverSpec {
  val PowerSet = TermDefinition(
    "powerSet",
    Seq(x),
    "Power Set",
    Format("𝒫%0", requiresBrackets = false),
    Nil,
    φ)

  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet, PartialSubstitutions.empty) must beSome
    }

    "match a unary function to another application of the same function" in {
      PowerSet(Seq(x))
        .calculateSubstitutions(PowerSet(Seq(y)), PartialSubstitutions.empty)
        .mustEqual(Some(PartialSubstitutions(
          Map.empty,
          Map(x -> y),
          Map.empty,
          Map.empty)))
    }
  }

  "term apply match" should {
    "do nothing to a constant" in {
      EmptySet.applySubstitutions(Substitutions(
        Map(φ -> ψ),
        Map(x -> y))
      ) mustEqual Some(EmptySet)
    }

    "replace terms in a unary function" in {
      PowerSet(Seq(x)).applySubstitutions(
        Substitutions(
          Map(φ -> ψ),
          Map(x -> y))
      ) mustEqual Some(PowerSet(Seq(y)))
    }
  }

}
