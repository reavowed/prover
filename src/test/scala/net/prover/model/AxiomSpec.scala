package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.entries.Axiom
import org.specs2.mutable.Specification

class AxiomSpec extends Specification {

  def parseAxiom(text: String*): Axiom = {
    implicit val availableEntries = defaultAvailableEntries
    Axiom.parser.parseAndDiscard(text.mkString("\n"))
  }

  "axiom parser" should {
    "parse an axiom with no premises" in {
      parseAxiom(
        "Equality Is Reflexive",
        "termVariables (a 0)",
        "conclusion = a a"
      ) mustEqual Axiom(
        "Equality Is Reflexive",
        VariableDefinitions(Nil, Seq(VariableDefinition("a", 0, Nil))),
        Nil,
        Equals(a, a))
    }

    "parse an axiom with a single premise" in {
      parseAxiom(
        "Restate",
        "statementVariables (φ 0)",
        "premise φ",
        "conclusion φ"
      ) mustEqual Axiom(
        "Restate",
        VariableDefinitions(Seq(VariableDefinition("φ", 0, Nil)), Nil),
        Seq(φ),
        φ)
    }

    "parse an axiom with two premises" in {
      parseAxiom(
        "Eliminate Implication",
        "statementVariables (φ 0, ψ 0)",
        "premise → φ ψ",
        "premise φ",
        "conclusion ψ"
      ) mustEqual Axiom(
        "Eliminate Implication",
        VariableDefinitions(Seq(VariableDefinition("φ", 0, Nil), VariableDefinition("ψ", 0, Nil)), Nil),
        Seq(Implication(φ, ψ), φ),
        ψ)
    }
  }
}
