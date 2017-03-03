package net.prover.model

class TermDefinitionSpec extends ProverSpec {
  "term definition parser" should {
    "parse a term constant" in {
      val specification = TermSpecification("∅", Nil, Format.default("∅", Nil))
      TermDefinition.parser(defaultContext).parseAndDiscard("∅ () (∀ x ¬ ∈ x ∅)") mustEqual
        TermDefinition(
          specification,
          Nil,
          Nil,
          ForAll(x, Negation(ElementOf(x, DefinedTerm(Nil, specification)))))
    }

    "parse a term with premises" in {
      val specification = TermSpecification("intersection", Seq(Term), Format("⋂x", Seq("x"), requiresBrackets = false))
      TermDefinition.parser(defaultContext).parseAndDiscard(
        "intersection (x) format (⋂x) premises (¬ = x ∅) (∀ y ↔ ∈ y intersection x ∀ z → ∈ z x ∈ z y)"
      ) mustEqual TermDefinition(
        specification,
        Seq(Negation(Equals(x, EmptySet))),
        Seq(x),
        ForAll(y, Equivalence(
          ElementOf(y, DefinedTerm(Seq(x), specification)),
          ForAll(z, Implication(ElementOf(z, x), ElementOf(z, y)))))
      )
    }
  }
}
