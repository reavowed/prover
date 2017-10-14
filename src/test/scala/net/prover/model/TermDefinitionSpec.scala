package net.prover.model

import net.prover.model.entries.TermDefinition
import net.prover.model.expressions.{DefinedTerm, FunctionParameter}

class TermDefinitionSpec extends ProverSpec {
  "term definition parser" should {
    "parse a term constant" in {
      val definition = TermDefinition.parser("", "").parseAndDiscard("∅ () (∀ x ¬ ∈ x _)")
      definition mustEqual
        TermDefinition(
          "∅",
          Nil,
          Nil,
          "∅",
          Format.default("∅", Nil),
          Nil,
          ForAll.!("x")(Negation.!!(ElementOf.!!(FunctionParameter("x", 0, 2), FunctionParameter("_", 0, 1, 2)))),
          "",
          "")
      definition.definingStatement mustEqual
        ForAll("x")(Negation.!(ElementOf.!(FunctionParameter("x", 0, 1), DefinedTerm(Nil, definition, 0)(Nil).^)))
    }

    "parse a term with premises" in {
      TermDefinition.parser("", "").parseAndDiscard(
        "intersection (a) format (⋂a) premises (¬ = a ∅) (∀ x ↔ ∈ x _ ∀ y → ∈ y a ∈ x y)"
      ) mustEqual TermDefinition(
        "intersection",
        Nil,
        Seq(a),
        "intersection",
        Format("⋂%0", requiresBrackets = false),
        Seq(Negation(Equals(a, EmptySet))),
        ForAll.!("x")(Equivalence.!!(
          ElementOf.!!(FunctionParameter("x", 0, 2), FunctionParameter.anonymous(0, 1, 2)),
          ForAll.!!("y")(Implication.!!!(
            ElementOf.!!!(FunctionParameter("y", 0, 3), a.^^^),
            ElementOf.!!!(FunctionParameter("x", 0, 2, 3), FunctionParameter("y", 0, 3)))))),
        "",
        "")
    }
  }
}
