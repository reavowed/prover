package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.ChapterEntry
import net.prover.types.model.entries.TypeDefinition
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class TypeDefinitionSpec extends Specification {

  implicit val entryContext = defaultEntryContext

  private def testParsingAndSerialization(typeDefinition: TypeDefinition)(implicit entryContext: EntryContext): MatchResult[Any] = {
    val serializedDefinition = typeDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(entryContext).parseAndDiscard(serializedDefinition)
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
