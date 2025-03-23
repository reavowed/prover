package net.prover.model

import net.prover.books.reading.ProofFileReader
import net.prover.model.TestDefinitions.{*, given}
import net.prover.model.entries.StatementDefinitionEntry
import org.specs2.mutable.Specification

class StatementDefinitionSpec extends Specification {

  "statement definition parser" should {
    def parseStatementDefinition(text: String): StatementDefinitionEntry = {
      given availableEntries: AvailableEntries = defaultAvailableEntries
      StatementDefinitionEntry.parser.parseAndDiscard(text)
    }

    "parse definition" in {
      parseStatementDefinition(
        "∧ (φ ψ) definition (¬ → φ ¬ ψ)"
      ).definingStatement must beSome(Negation(Implication(φ, Negation(ψ))))
    }
  }
}
