package net.prover.model

import net.prover.model.entries.TermDefinition
import net.prover.model.expressions.{DefinedTerm, FunctionParameter}

class TermDefinitionSpec extends ProverSpec {
  "term definition parser" should {
    "parse a term constant" in {
      val definition = TermDefinition.parser.parseAndDiscard("∅ () (∀ x ¬ ∈ x _)")
      definition mustEqual
        TermDefinition(
          "∅",
          Nil,
          Nil,
          None,
          Format.default("∅", Nil),
          Nil,
          ForAll("x")(Negation(ElementOf(FunctionParameter("x", 0, 0), FunctionParameter("_", 0, 1)))))
      definition.definingStatement mustEqual
        ForAll("x")(Negation(ElementOf(FunctionParameter("x", 0, 0), DefinedTerm(Nil, definition)(Nil))))
    }

    "parse a term with premises" in {
      TermDefinition.parser.parseAndDiscard(
        "intersection (a) format (⋂a) premises (¬ = a ∅) (∀ x ↔ ∈ x _ ∀ y → ∈ y a ∈ x y)"
      ) mustEqual TermDefinition(
        "intersection",
        Nil,
        Seq(a),
        None,
        Format.Explicit("⋂%0", "⋂a", requiresBrackets = false),
        Seq(Negation(Equals(a, EmptySet))),
        ForAll("x")(Equivalence(
          ElementOf(FunctionParameter("x", 0, 0), FunctionParameter.anonymous(0, 1)),
          ForAll("y")(Implication(
            ElementOf(FunctionParameter("y", 0, 0), a),
            ElementOf(FunctionParameter("x", 0, 1), FunctionParameter("y", 0, 0)))))))
    }
  }
}
