package net.prover.model

import net.prover.model.entries.StatementDefinition

class StatementDefinitionSpec extends ProverSpec {
  "statement definition parser" should {
    def parseStatementDefinition(text: String): StatementDefinition = {
      StatementDefinition.parser.parseAndDiscard(text)
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
      ).definingStatement must beSome(Negation(Implication(φ, Negation(ψ))))
    }

    "parse bound variables" in {
      parseStatementDefinition(
        "∀ (x φ) boundVariables (x)"
      ).boundVariables mustEqual Set(x)
    }

    "infer bound variables from a defining statement" in {
      StatementDefinition.parser.parseAndDiscard(
        "∃ (x φ) format ((∃{}){}) definition (¬ ∀ x ¬ φ)"
      ).boundVariables mustEqual Set(x)
    }

    "infer bound variables that are not fully bound in the defining statement" in {
      StatementDefinition.parser.parseAndDiscard(
        "∃! (x φ) format ((∃!{}){}) definition (∧ ∃ x φ ∀ y ∀ z → ∧ sub y x φ sub z x φ = y z)"
      ).boundVariables mustEqual Set(x)
    }
  }

  "a statement definition" should {
    "calculate bound variables when applying to subcomponents" in {
      ForAll(y, φ).boundVariables mustEqual Set(y)
    }
  }
}
