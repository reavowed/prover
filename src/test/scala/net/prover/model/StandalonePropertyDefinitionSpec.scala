package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.{ChapterEntry, StandalonePropertyDefinition, TypeQualifierDefinition}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class StandalonePropertyDefinitionSpec extends Specification {

  implicit val entryContext = defaultEntryContext

  private def testParsingAndSerialization(standalonePropertyDefinition: StandalonePropertyDefinition)(implicit entryContext: EntryContext): MatchResult[Any] = {
    val serializedDefinition = standalonePropertyDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(entryContext).parseAndDiscard(serializedDefinition)
    reparsedDefinition must beSome(standalonePropertyDefinition)
  }

  "type qualifier definition parser" should {
    "serialize and parse a definition correctly" in {
      val definition = StandalonePropertyDefinition("prime", "a", None, Equals(a, a))
      testParsingAndSerialization(definition)
    }
  }
}
