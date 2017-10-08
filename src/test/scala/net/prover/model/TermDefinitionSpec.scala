package net.prover.model

import net.prover.model.entries.TermDefinition
import net.prover.model.expressions.{ConstantFunction, FunctionParameter, PlaceholderTerm}

class TermDefinitionSpec extends ProverSpec {
  "term definition parser" should {
    "parse a term constant" in {
      TermDefinition.parser("", "").parseAndDiscard("∅ () (∀ x ¬ ∈ x _)") mustEqual
        TermDefinition(
          "∅",
          Nil,
          "∅",
          Format.default("∅", Nil),
          Nil,
          ForAll("x")(Negation.!(ElementOf.!(FunctionParameter("x", 0), ConstantFunction(PlaceholderTerm, 1)))),
          "",
          "")
    }

    "parse a term with premises" in {
      val res = TermDefinition.parser("", "").parseAndDiscard(
        "intersection (x) format (⋂x) premises (¬ = x ∅) (∀ y ↔ ∈ y _ ∀ z → ∈ z x ∈ z y)"
      )
      res mustEqual TermDefinition(
        "intersection",
        Seq(x),
        "intersection",
        Format("⋂%0", requiresBrackets = false),
        Seq(Negation(Equals(x, EmptySet))),
        ForAll("y")(Equivalence.!(
          ElementOf.!(FunctionParameter("y", 0), ConstantFunction(PlaceholderTerm, 1)),
          ForAll.!("z")(Implication.!!(
            ElementOf.!!(FunctionParameter("z", 0, 1, 2), ConstantFunction(x, 2)),
            ElementOf.!!(FunctionParameter("z", 0, 1, 2), FunctionParameter("y", 0, 2)))))),
        "",
        "")
    }
  }
}
