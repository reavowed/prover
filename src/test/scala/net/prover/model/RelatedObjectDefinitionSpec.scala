package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.definitions.Qualifier
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.{ChapterEntry, ParentTypeConditions, RelatedObjectDefinition, TypeDefinition}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class RelatedObjectDefinitionSpec extends Specification {

  private def testParsingAndSerialization(relatedObjectDefinition: RelatedObjectDefinition)(implicit entryContext: EntryContext): MatchResult[Any] = {
    val serializedDefinition = relatedObjectDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(entryContext).parseAndDiscard(serializedDefinition)
    reparsedDefinition must beSome(relatedObjectDefinition)
  }

  val e = TermVariablePlaceholder("e", 0)
  val f = TermVariablePlaceholder("f", 1)
  val A = TermVariablePlaceholder("A", 2)

  val binaryOperationDefinition = TypeDefinition("binaryOperation", "f", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, true))), Some("binary Operation"), φ)

  "related object definition parser" should {
    "serialize and parse a definition correctly" in {
      val definition = RelatedObjectDefinition(
        "identity",
        "e",
        ParentTypeConditions(
          binaryOperationDefinition,
          None,
          None,
          None,
          ConjunctionDefinition),
        None,
        ForAllIn("a", A)(Conjunction(Equals(Apply(f, Pair($, e)), $), Equals(Apply(f, Pair(e, $)), $))))
      testParsingAndSerialization(definition)(defaultEntryContext.addEntry(binaryOperationDefinition))
    }
  }
}
