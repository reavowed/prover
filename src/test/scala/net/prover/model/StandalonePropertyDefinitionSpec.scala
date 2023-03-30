package net.prover.model

import net.prover.books.reading.ProofFileReader
import net.prover.model.TestDefinitions._
import net.prover.model.entries.{ChapterEntry, StandalonePropertyDefinition, TypeQualifierDefinition}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class StandalonePropertyDefinitionSpec extends Specification {

  implicit val availableEntries = defaultAvailableEntries

  private def testParsingAndSerialization(standalonePropertyDefinition: StandalonePropertyDefinition)(implicit availableEntries: AvailableEntries): MatchResult[Any] = {
    val serializedDefinition = standalonePropertyDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(availableEntries, mock[ProofFileReader]).parseAndDiscard(serializedDefinition)
    reparsedDefinition must beSome(standalonePropertyDefinition)
  }

  "type qualifier definition parser" should {
    "serialize and parse a definition correctly" in {
      val definition = StandalonePropertyDefinition("prime", "a", None, Equals(a, a))
      testParsingAndSerialization(definition)
    }
  }
}
