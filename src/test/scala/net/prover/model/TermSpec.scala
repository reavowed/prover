package net.prover.model

import shapeless.{::, HNil}

class TermSpec extends ProverSpec {
  val EmptySet: Term = ComponentTypeList.empty.termDefinition("∅", "∅", None).apply(HNil)
  val PowerSet: TermDefinition[Term :: HNil] = ComponentTypeList.withTerm(ComponentTypeList.empty).termDefinition("powerSet", "𝒫{}", None)

  "two equally defined term constants should be equal" in {
    val one = ComponentTypeList.empty.termDefinition("∅", "∅", None).apply(HNil)
    val two = ComponentTypeList.empty.termDefinition("∅", "∅", None).apply(HNil)
    one mustEqual two
  }

  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet) must beSome
    }

    "match a unary function to another application of the same function" in {
      PowerSet(TermVariable(1) :: HNil)
        .calculateSubstitutions(PowerSet(TermVariable(2) :: HNil))
        .mustEqual(Some(Substitutions(
          Map.empty,
          Map(TermVariable(1) -> TermVariable(2)))))
    }
  }
  "term apply match" should {
    "do nothing to a constant" in {
      EmptySet.applySubstitutions(Substitutions(
        Map(StatementVariable(1) -> StatementVariable(2)),
        Map(TermVariable(1) -> TermVariable(2)))
      ) mustEqual EmptySet
    }

    "replace terms in a unary function" in {
      PowerSet(TermVariable(1) :: HNil).applySubstitutions(
        Substitutions(
          Map(StatementVariable(1) -> StatementVariable(2)),
          Map(TermVariable(1) -> TermVariable(2)))
      ) mustEqual PowerSet(TermVariable(2) :: HNil)
    }
  }

}
