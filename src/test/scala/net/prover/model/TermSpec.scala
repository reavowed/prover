package net.prover.model

import shapeless.{::, HNil}

class TermSpec extends ProverSpec {
  val PowerSet = TermSpecification("powerSet", Seq(Term), "𝒫{}", requiresBrackets = false)

  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet) must beSome
    }

    "match a unary function to another application of the same function" in {
      PowerSet(Seq(TermVariable("z")))
        .calculateSubstitutions(PowerSet(Seq(TermVariable("y"))))
        .mustEqual(Some(Substitutions(
          Map.empty,
          Map(TermVariable("z") -> TermVariable("y")))))
    }
  }
  "term apply match" should {
    "do nothing to a constant" in {
      EmptySet.applySubstitutions(Substitutions(
        Map(StatementVariable(1) -> StatementVariable(2)),
        Map(TermVariable("z") -> TermVariable("y")))
      ) mustEqual EmptySet
    }

    "replace terms in a unary function" in {
      PowerSet(Seq(TermVariable("z"))).applySubstitutions(
        Substitutions(
          Map(StatementVariable(1) -> StatementVariable(2)),
          Map(TermVariable("z") -> TermVariable("y")))
      ) mustEqual PowerSet(Seq(TermVariable("y")))
    }
  }

}
