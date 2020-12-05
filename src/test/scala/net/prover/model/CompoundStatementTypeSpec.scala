package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.structure.model.entries.CompoundStatementDefinitionEntry
import org.specs2.mutable.Specification

class CompoundStatementTypeSpec extends Specification {

  implicit val entryContext = defaultEntryContext

  "statement definition parser" should {
    def parseStatementDefinition(text: String): CompoundStatementDefinitionEntry = {
      CompoundStatementDefinitionEntry.parser.parseAndDiscard(text)
    }

    "parse definition" in {
      parseStatementDefinition(
        "∧ (φ ψ) definition (¬ → φ ¬ ψ)"
      ).definingStatement must beSome(Negation(Implication(φ, Negation(ψ))))
    }
  }
}
