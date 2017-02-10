package net.prover.model

class StatementDefinitionSpec extends ProverSpec {
  "a statement definition" should {
    "infer bound variables from a defining statement" in {
      StatementDefinition.parser(defaultContext).parseAndDiscard(
        "∃ (term statement) ((∃{}){}) (1 1) (¬ ∀ 1 ¬ 1) ()"
      ).boundVariables mustEqual Seq(TermVariable("z"))
    }

    "calculate bound variables when applying to subcomponents" in {
      ForAll(TermVariable("y"), StatementVariable(1))
          .allBoundVariables mustEqual Seq(TermVariable("y"))
    }

    "parse distinct variables" in {
      StatementDefinition.parser(defaultContext).parseAndDiscard(
        "∃! (term statement) ((∃!{}){}) (1 1) (∃ 2 ∀ 1 ↔ 1 = 1 2) (2 1)"
      ).distinctVariables mustEqual DistinctVariables(Map(TermVariable("y") -> Variables(Seq(1), Nil)))
    }
  }
}
