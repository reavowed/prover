package net.prover.model

import net.prover.books.reading.ProofFileReader
import net.prover.model.TestDefinitions._
import net.prover.model.entries.{ChapterEntry, TypeDefinition}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class TypeDefinitionSpec extends Specification {

  implicit val availableEntries = defaultAvailableEntries

  private def testParsingAndSerialization(typeDefinition: TypeDefinition)(implicit availableEntries: AvailableEntries): MatchResult[Any] = {
    val serializedDefinition = typeDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser.parseAndDiscard(serializedDefinition)
    reparsedDefinition mustEqual typeDefinition
  }

  "type qualifier definition parser" should {
    "serialize and parse a definition correctly" in {
      testParsingAndSerialization(Function)
      testParsingAndSerialization(Relation)
      testParsingAndSerialization(BinaryOperation)
    }
  }
}
