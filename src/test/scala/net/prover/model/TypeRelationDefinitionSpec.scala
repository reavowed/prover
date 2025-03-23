package net.prover.model

import net.prover.model.TestDefinitions.{*, given}
import net.prover.model.entries.{ChapterEntry, TypeRelationDefinition}
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class TypeRelationDefinitionSpec extends Specification {

  given availableEntries: AvailableEntries = defaultAvailableEntries

  private def testParsingAndSerialization(
    typeRelationDefinition: TypeRelationDefinition)(
    using availableEntries: AvailableEntries
  ): Result = {
    val serializedDefinition = typeRelationDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser.parseAndDiscard(serializedDefinition)
    reparsedDefinition must beEqualTo(typeRelationDefinition)
  }

  "type relation definition parser" should {
    "serialize and parse a definition correctly" in {
      testParsingAndSerialization(Distributive)
    }
  }
}
