package net.prover.model

class TermDefinitionSpec extends ProverSpec {
  "term definition parser" should {
    "parse a term constant" in {
      val specification = TermSpecification("∅", Nil, Format("∅", requiresBrackets = false))
      TermDefinition.parser(defaultContext).parseAndDiscard("∅ () () () () (∀ 1 ¬ ∈ 1 ∅)") mustEqual
        TermDefinition(
          specification,
          Nil,
          Nil,
          ForAll("z", Negation(ElementOf("z", DefinedTerm(Nil, specification)))))
    }

    "parse a term with premises" in {
      val specification = TermSpecification("intersection", Seq(Term), Format("⋂{}", requiresBrackets = false))
      TermDefinition.parser(defaultContext).parseAndDiscard(
        "intersection (term) (⋂{}) (1) (¬ = 1 ∅) (∀ 2 ↔ ∈ 2 intersection 1 ∀ 3 → ∈ 3 1 ∈ 2 3)"
      ) mustEqual TermDefinition(
        specification,
        Seq(Negation(Equals("z", EmptySet))),
        Seq(TermVariable("z")),
        ForAll("y", Equivalence(
          ElementOf("y", DefinedTerm(Seq(TermVariable("z")), specification)),
          ForAll("x", Implication(ElementOf("x", "z"), ElementOf("y", "x")))))
      )
    }
  }
}
