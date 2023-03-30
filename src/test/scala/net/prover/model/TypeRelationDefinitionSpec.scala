package net.prover.model

import net.prover.books.reading.ProofFileReader
import net.prover.model.TestDefinitions._
import net.prover.model.entries.{ChapterEntry, TypeQualifierDefinition, TypeRelationDefinition}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class TypeRelationDefinitionSpec extends Specification {

  implicit val availableEntries = defaultAvailableEntries

  private def testParsingAndSerialization(typeRelationDefinition: TypeRelationDefinition)(implicit availableEntries: AvailableEntries): MatchResult[Any] = {
    val serializedDefinition = typeRelationDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(availableEntries, mock[ProofFileReader]).parseAndDiscard(serializedDefinition)
    reparsedDefinition must beSome(typeRelationDefinition)
  }

  "type relation definition parser" should {
    "serialize and parse a definition correctly" in {
      testParsingAndSerialization(Distributive)
    }
  }
}
