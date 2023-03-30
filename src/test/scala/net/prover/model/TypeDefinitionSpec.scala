package net.prover.model

import net.prover.books.reading.ProofFileReader
import net.prover.model.TestDefinitions._
import net.prover.model.entries.{ChapterEntry, TypeDefinition}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class TypeDefinitionSpec extends Specification {

  implicit val entryContext = defaultEntryContext

  private def testParsingAndSerialization(typeDefinition: TypeDefinition)(implicit entryContext: EntryContext): MatchResult[Any] = {
    val serializedDefinition = typeDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(entryContext, mock[ProofFileReader]).parseAndDiscard(serializedDefinition)
    reparsedDefinition must beSome(typeDefinition)
  }

  "type qualifier definition parser" should {
    "serialize and parse a definition correctly" in {
      testParsingAndSerialization(Function)
      testParsingAndSerialization(Relation)
      testParsingAndSerialization(BinaryOperation)
    }
  }
}
