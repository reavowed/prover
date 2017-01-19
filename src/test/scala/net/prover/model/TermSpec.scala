package net.prover.model

import shapeless.{::, HNil}

class TermSpec extends ProverSpec {
  val PowerSet: TermSpecification[Term :: HNil] = ComponentTypeList.withTerm(ComponentTypeList.empty)
    .termSpecification("powerSet", "𝒫{}", requiresBrackets = false)

  "two equally defined term constants should be equal" in {
    val one = ComponentTypeList.empty.termSpecification("∅", "∅", requiresBrackets = false).apply(HNil)
    val two = ComponentTypeList.empty.termSpecification("∅", "∅", requiresBrackets = false).apply(HNil)
    one mustEqual two
  }

  "term match" should {
    "match a constant term to itself" in {
      EmptySet.calculateSubstitutions(EmptySet) must beSome
    }

    "match a unary function to another application of the same function" in {
      PowerSet(TermVariable("z") :: HNil)
        .calculateSubstitutions(PowerSet(TermVariable("y") :: HNil))
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
      PowerSet(TermVariable("z") :: HNil).applySubstitutions(
        Substitutions(
          Map(StatementVariable(1) -> StatementVariable(2)),
          Map(TermVariable("z") -> TermVariable("y")))
      ) mustEqual PowerSet(TermVariable("y") :: HNil)
    }
  }

}
