package net.prover.model

import net.prover.model.TestDefinitions.{*, given}
import net.prover.model.entries.{ChapterEntry, TypeQualifierDefinition}
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class TypeQualifierDefinitionSpec extends Specification {

  given availableEntries: AvailableEntries = defaultAvailableEntries

  private def testParsingAndSerialization(
    typeQualifierDefinition: TypeQualifierDefinition)(
    using availableEntries: AvailableEntries
  ): Result = {
    val serializedDefinition = typeQualifierDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser.parseAndDiscard(serializedDefinition)
    reparsedDefinition must beEqualTo(typeQualifierDefinition)
  }

  "type qualifier definition parser" should {
    "serialize and parse a definition correctly" in {
      testParsingAndSerialization(FunctionFrom)
      testParsingAndSerialization(BinaryOperationOn)
    }
  }
}
