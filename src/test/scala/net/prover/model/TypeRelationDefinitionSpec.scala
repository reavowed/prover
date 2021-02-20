package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.ChapterEntry
import net.prover.types.model.entries.TypeRelationDefinition
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class TypeRelationDefinitionSpec extends Specification {

  implicit val entryContext = defaultEntryContext

  private def testParsingAndSerialization(typeRelationDefinition: TypeRelationDefinition)(implicit entryContext: EntryContext): MatchResult[Any] = {
    val serializedDefinition = typeRelationDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(entryContext).parseAndDiscard(serializedDefinition)
    reparsedDefinition must beSome(typeRelationDefinition)
  }

  "type relation definition parser" should {
    "serialize and parse a definition correctly" in {
      testParsingAndSerialization(Distributive)
    }
  }
}
