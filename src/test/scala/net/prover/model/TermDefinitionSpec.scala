package net.prover.model

import net.prover.model.entries.{ChapterEntry, TermDefinition}
import net.prover.model.expressions.{DefinedTerm, FunctionParameter}

class TermDefinitionSpec extends ProverSpec {

  private def parse(text: String): TermDefinition = {
    TermDefinition.parser.parseAndDiscard(text)
  }

  "term definition parser" should {
    "parse a term constant" in {
      val definition = parse("∅ () (∀ x ¬ ∈ x _)")
      definition mustEqual
        TermDefinition(
          "∅",
          Nil,
          Nil,
          None,
          Format.default("∅", Nil),
          Nil,
          ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))),
          None,
          Nil)
      definition.definingStatement mustEqual
        ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), DefinedTerm(Nil, definition)(Nil))))
    }

    "parse a term with premises" in {
      parse(
        "intersection (a) format (⋂a) premises (¬ = a ∅) (∀ x ↔ ∈ x _ ∀ y → ∈ y a ∈ x y)"
      ) mustEqual TermDefinition(
        "intersection",
        Nil,
        Seq(a),
        None,
        Format.Explicit("⋂%0", "⋂a", requiresBrackets = false),
        Seq(Negation(Equals(a, EmptySet))),
        ForAll("x")(Equivalence(
          ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)),
          ForAll("y")(Implication(
            ElementOf(FunctionParameter(0, 0), a),
            ElementOf(FunctionParameter(0, 1), FunctionParameter(0, 0)))))),
        None,
        Nil)
    }
  }
}
