package net.prover.model

import net.prover.model.TestDefinitions.{*, given}
import net.prover.model.entries.{ChapterEntry, StandalonePropertyDefinition}
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class StandalonePropertyDefinitionSpec extends Specification {

  given availableEntries: AvailableEntries = defaultAvailableEntries

  private def testParsingAndSerialization(
    standalonePropertyDefinition: StandalonePropertyDefinition)(
    using availableEntries: AvailableEntries
  ): Result = {
    val serializedDefinition = standalonePropertyDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser.parseAndDiscard(serializedDefinition)
    reparsedDefinition must beEqualTo(standalonePropertyDefinition)
  }

  "type qualifier definition parser" should {
    "serialize and parse a definition correctly" in {
      val definition = StandalonePropertyDefinition("prime", "a", None, Equals(a, a))
      testParsingAndSerialization(definition)
    }
  }
}
