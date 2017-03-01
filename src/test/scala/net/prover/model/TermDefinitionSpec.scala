package net.prover.model

class TermDefinitionSpec extends ProverSpec {
  "term definition parser" should {
    "parse a term constant" in {
      val specification = TermSpecification("∅", Nil, Format("∅", requiresBrackets = false))
      TermDefinition.parser(defaultContext).parseAndDiscard("∅ () () () () (∀ x ¬ ∈ x ∅)") mustEqual
        TermDefinition(
          specification,
          Nil,
          Nil,
          ForAll(x, Negation(ElementOf(x, DefinedTerm(Nil, specification)))))
    }

    "parse a term with premises" in {
      val specification = TermSpecification("intersection", Seq(Term), Format("⋂{}", requiresBrackets = false))
      TermDefinition.parser(defaultContext).parseAndDiscard(
        "intersection (term) (⋂{}) (x) (¬ = x ∅) (∀ y ↔ ∈ y intersection x ∀ z → ∈ z x ∈ z y)"
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
