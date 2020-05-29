package net.prover.model

import org.specs2.mutable.Specification
import TestDefinitions._
import net.prover.model.entries.Axiom
import net.prover.model.expressions.{StatementVariable, TermVariable}

class InferenceSpec extends Specification {
  "inference id" should {
    "be independent of inference name" in {
      createInference("XXX", Seq(φ), ψ).id mustEqual createInference("YYY", Seq(φ), ψ).id
    }
    "be independent of bound variable names" in {
      createInference("XXX", Nil, ForAll("x")(φ($))).id mustEqual createInference("YYY", Nil, ForAll("x")(φ($))).id
    }
    "be independent of variable names" in {
      Axiom(
        "XXX",
        VariableDefinitions(Seq(VariableDefinition("φ", 1, Nil), VariableDefinition("ψ", 0, Nil)), Seq(VariableDefinition("a", 0, Nil))),
        Seq(StatementVariable(0, Nil)),
        StatementVariable(1, Seq(TermVariable(0, Nil)))
      ).id mustEqual Axiom(
        "YYY",
        VariableDefinitions(Seq(VariableDefinition("ψ", 1, Nil), VariableDefinition("φ", 0, Nil)), Seq(VariableDefinition("b", 0, Seq("irrelevant-attribute")))),
        Seq(StatementVariable(0, Nil)),
        StatementVariable(1, Seq(TermVariable(0, Nil)))
      ).id
    }
  }
}
