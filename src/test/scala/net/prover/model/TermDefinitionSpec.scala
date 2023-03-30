package net.prover.model

import net.prover.books.reading.ProofFileReader
import net.prover.model.TestDefinitions._
import net.prover.model.entries.{ChapterEntry, TermDefinitionEntry}
import net.prover.model.expressions.FunctionParameter
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class TermDefinitionSpec extends Specification {

  private def testParsingAndSerialization(termDefinition: TermDefinitionEntry): MatchResult[Any] = {
    val serializedDefinition = termDefinition.serializedLines.mkString("\n")
    val reparsedDefinition = ChapterEntry.parser(defaultAvailableEntries, mock[ProofFileReader]).parseAndDiscard(serializedDefinition)
    reparsedDefinition must beSome(termDefinition)
  }

  "term definition parser" should {
    "parse a term constant" in {
      testParsingAndSerialization(
        TermDefinitionEntry(
          "∅",
          Nil,
          Nil,
          None,
          None,
          Format.default(Nil, Nil),
          Nil,
          ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))),
          None,
          Nil,
          Nil))
    }

    "parse a term with premises" in {
      testParsingAndSerialization(
        TermDefinitionEntry(
          "intersection",
          Nil,
          Seq(a),
          None,
          None,
          Format.Explicit("%0%1", "⋂a", 2, requiresBrackets = false, requiresComponentBrackets = true),
          Seq(Negation(Equals(a, EmptySet))),
          ForAll("x")(Equivalence(
            ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)),
            ForAll("y")(Implication(
              ElementOf(FunctionParameter(0, 0), a),
              ElementOf(FunctionParameter(0, 1), FunctionParameter(0, 0)))))),
          None,
          Nil,
          Nil))
    }
    "parse a term with a disambiguator" in {
      testParsingAndSerialization(
        TermDefinitionEntry(
          "0",
          Nil,
          Nil,
          Some("ℕ"),
          None,
          Format.default(Nil, Nil),
          Nil,
          Equals($, EmptySet),
          None,
          Nil,
          Nil))
    }
    "parse a term with a disambiguator adder" in {
      testParsingAndSerialization(
        TermDefinitionEntry(
          "⍳",
          Nil,
          Nil,
          Some("ℤ"),
          None,
          Format.default(Nil, Nil),
          Nil,
          Equals($, EmptySet),
          None,
          Nil,
          Seq(DisambiguatorAdder(Apply.template($.template, a.template), "ℤ"))))
    }
  }
}
