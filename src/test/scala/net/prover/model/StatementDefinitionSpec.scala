package net.prover.model

class StatementDefinitionSpec extends ProverSpec {
  "statement definition parser" should {
    def parseStatementDefinition(text: String): StatementDefinition = {
      StatementDefinition.parser(defaultContext).parseAndDiscard(text)
    }

    "fill in default format for a constant statement" in {
      parseStatementDefinition("⊤ ()").apply().html mustEqual "⊤"
    }

    "fill in default format for a unary statement" in {
      parseStatementDefinition("¬ (φ)").apply(φ).html mustEqual "¬φ"
    }

    "fill in default format for a binary statement" in {
      parseStatementDefinition("→ (φ ψ)").apply(φ, ψ).html mustEqual "φ → ψ"
    }

    "use a specified format" in {
      parseStatementDefinition(
        "∀ (x φ) format ((∀x)φ)"
      ).apply(y, ψ).html mustEqual "(∀y)ψ"
    }

    "parse definition" in {
      parseStatementDefinition(
        "∧ (φ ψ) definition (¬ → φ ¬ ψ)"
      ).definingStatement mustEqual Some(Negation(Implication(φ, Negation(ψ))))
    }

    "parse bound variables" in {
      parseStatementDefinition(
        "∀ (x φ) boundVariables (x)"
      ).boundVariables mustEqual Seq(x)
    }

    "infer bound variables from a defining statement" in {
      StatementDefinition.parser(defaultContext).parseAndDiscard(
        "∃ (x φ) format ((∃{}){}) definition (¬ ∀ x ¬ φ)"
      ).boundVariables mustEqual Seq(x)
    }

    "parse distinct variables" in {
      parseStatementDefinition(
        "∃! (x φ) format ((∃!{}){}) definition (∃ y ∀ x ↔ φ = x y) distinct-variables (y φ)"
      ).distinctVariables mustEqual Map(y -> Variables(Set(φ), Set.empty))
    }
  }

  "a statement definition" should {
    "calculate bound variables when applying to subcomponents" in {
      ForAll(y, φ).boundVariables mustEqual Set(y)
    }
  }
}
