package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.entries.{ChapterEntry, TypeQualifierDefinition}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class TypeQualifierDefinitionSpec extends Specification {

  implicit val entryContext = defaultEntryContext

  private def testParsingAndSerialization(typeQualifierDefinition: TypeQualifierDefinition)(implicit entryContext: EntryContext): MatchResult[Any] = {
    val serializedDefinition = typeQualifierDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(entryContext).parseAndDiscard(serializedDefinition)
    reparsedDefinition must beSome(typeQualifierDefinition)
  }

  "type qualifier definition parser" should {
    "serialize and parse a definition correctly" in {
      testParsingAndSerialization(FunctionFrom)
      testParsingAndSerialization(BinaryOperationOn)
    }
  }
}
