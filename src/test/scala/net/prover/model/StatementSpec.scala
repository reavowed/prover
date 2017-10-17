package net.prover.model

import net.prover.model.expressions._

class StatementSpec extends ProverSpec {

  def parseStatement(line: String): Statement = {
    Statement.parser(defaultContext).parseAndDiscard(line)
  }

  def parseStatementList(line: String): Seq[Statement] = {
    Statement.listParser(defaultContext).parseAndDiscard(line)
  }

  "statement parser" should {
    "parse a statement variable" in {
      parseStatement("φ") mustEqual φ
    }

    "parse a binary connective" in {
      parseStatement("→ φ ψ") mustEqual Implication(φ, ψ)
    }

    "parse a nested binary connective" in {
      parseStatement("→ → φ ψ χ") mustEqual
        Implication(Implication(φ, ψ), χ)
    }

    "parse a bound predicate application" in {
      parseStatement("∀ y with y χ") mustEqual ForAll("y")(χ.!(FunctionParameter("y", 0)))
    }

    "parse a bound defined statement" in {
      parseStatement("∀ y ↔ ∈ y a ∈ y b") mustEqual ForAll("y")(Equivalence.!(
        ElementOf.!(FunctionParameter("y", 0), a.^),
        ElementOf.!(FunctionParameter("y", 0), b.^)))
    }

    "parse an empty list" in {
      parseStatementList("()") mustEqual Nil
    }

    "parse a list with a single statement" in {
      parseStatementList("(φ)") mustEqual Seq(φ)
    }

    "parse a list with multiple statements" in {
      parseStatementList("(φ, ψ, χ)") mustEqual Seq(φ, ψ, χ)
    }
  }

  "statement condensing" should {
    "condense a statement variable with a known substitution to a matching compound statement" in {
      val premise = φ
      val premiseSubstitutions = Substitutions(
        Map(φ -> Conjunction(φ, ψ)), Map.empty)
      val conclusion = Conjunction(χ, φ)
      val conclusionSubstitutions = Substitutions.empty
      premise.condense(conclusion, premiseSubstitutions, conclusionSubstitutions) must beSome((
        Substitutions(statements = Map(φ -> Conjunction(φ, ψ))),
        Substitutions(statements = Map(χ -> φ, φ -> ψ))))
    }

    "condense a compound statement to a statement variable with a known matching substitution" in {
      val premise = φ
      val premiseSubstitutions = Substitutions(Map(φ -> Conjunction(φ, ψ)), Map.empty)
      val conclusion = Conjunction(χ, φ)
      val conclusionSubstitutions = Substitutions.empty
      conclusion.condense(premise, conclusionSubstitutions, premiseSubstitutions) must beSome((
        Substitutions(statements = Map(χ -> φ, φ -> ψ)),
        Substitutions(statements = Map(φ -> Conjunction(φ, ψ)))))
    }
  }
}
