package net.prover.model

import net.prover.model.entries.{ChapterEntry, StatementDefinition}

class StatementDefinitionSpec extends ProverSpec {
  "statement definition parser" should {
    def parseStatementDefinition(text: String): StatementDefinition = {
      StatementDefinition.parser(s => (s.formatAsKey, stubChapter.key)).parseAndDiscard(text)
    }

    "parse definition" in {
      parseStatementDefinition(
        "∧ (φ ψ) definition (¬ → φ ¬ ψ)"
      ).definingStatement must beSome(Negation(Implication(φ, Negation(ψ))))
    }
  }
}
