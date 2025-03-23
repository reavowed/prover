package net.prover.model

import net.prover.books.reading.ProofFileReader
import net.prover.model.TestDefinitions.{*, given}
import net.prover.model.entries.{ChapterEntry, TypeDefinition}
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class TypeDefinitionSpec extends Specification {

  given availableEntries: AvailableEntries = defaultAvailableEntries

  private def testParsingAndSerialization(
    typeDefinition: TypeDefinition)(
    using availableEntries: AvailableEntries
  ): Result = {
    val serializedDefinition = typeDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser.parseAndDiscard(serializedDefinition)
    reparsedDefinition must beEqualTo(typeDefinition)
  }

  "type qualifier definition parser" should {
    "serialize and parse a definition correctly" in {
      testParsingAndSerialization(Function)
      testParsingAndSerialization(Relation)
      testParsingAndSerialization(BinaryOperation)
    }
  }
}
