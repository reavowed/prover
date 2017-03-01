package net.prover.model

class StatementDefinitionSpec extends ProverSpec {
  "a statement definition" should {
    "infer bound variables from a defining statement" in {
      StatementDefinition.parser(defaultContext).parseAndDiscard(
        "∃ (term statement) ((∃{}){}) (x φ) (¬ ∀ x ¬ φ) ()"
      ).boundVariables mustEqual Seq(x)
    }

    "calculate bound variables when applying to subcomponents" in {
      ForAll(y, φ).allBoundVariables mustEqual Seq(y)
    }

    "parse distinct variables" in {
      StatementDefinition.parser(defaultContext).parseAndDiscard(
        "∃! (term statement) ((∃!{}){}) (x φ) (∃ y ∀ x ↔ φ = x y) (y φ)"
      ).distinctVariables mustEqual DistinctVariables(Map(y -> Variables(Seq(φ), Nil)))
    }
  }
}
