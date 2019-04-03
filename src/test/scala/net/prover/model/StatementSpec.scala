package net.prover.model

import net.prover.model.expressions._

class StatementSpec extends ProverSpec {

  def parseStatement(line: String): Statement = {
    Statement.parser.parseAndDiscard(line)
  }

  def parseStatementList(line: String): Seq[Statement] = {
    Statement.listParser.parseAndDiscard(line)
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
      parseStatement("∀ y with y χ") mustEqual ForAll("y")(χ(FunctionParameter(0, 0)))
    }

    "parse a bound defined statement" in {
      parseStatement("∀ y ↔ ∈ y a ∈ y b") mustEqual ForAll("y")(Equivalence(
        ElementOf(FunctionParameter(0, 0), a),
        ElementOf(FunctionParameter(0, 0), b)))
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
}
