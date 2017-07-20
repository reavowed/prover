package net.prover.model

import net.prover.model.components.PlaceholderTerm
import net.prover.model.entries.TermDefinition

class TermDefinitionSpec extends ProverSpec {
  "term definition parser" should {
    "parse a term constant" in {
      TermDefinition.parser("", "").parseAndDiscard("∅ () (∀ x ¬ ∈ x _)") mustEqual
        TermDefinition(
          "∅",
          Nil,
          "∅",
          Format.default("∅", Nil),
          Nil,
          ForAll(x, Negation(ElementOf(x, PlaceholderTerm))),
          Set.empty,
          DistinctVariables.empty,
          "",
          "")
    }

    "parse a term with premises" in {
      TermDefinition.parser("", "").parseAndDiscard(
        "intersection (x) format (⋂x) premises (¬ = x ∅) (∀ y ↔ ∈ y _ ∀ z → ∈ z x ∈ z y)"
      ) mustEqual TermDefinition(
        "intersection",
        Seq(x),
        "intersection",
        Format("⋂%0", requiresBrackets = false),
        Seq(Negation(Equals(x, EmptySet))),
        ForAll(y, Equivalence(
          ElementOf(y, PlaceholderTerm),
          ForAll(z, Implication(ElementOf(z, x), ElementOf(z, y))))),
        Set.empty,
        DistinctVariables.empty,
        "",
        "")
    }
  }
}
