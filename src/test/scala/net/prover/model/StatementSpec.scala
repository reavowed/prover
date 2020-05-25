package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.expressions._
import org.specs2.mutable.Specification

class StatementSpec extends Specification {

  implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext(
    defaultEntryContext,
    VariableDefinitions(
      Seq(VariableDefinition("φ", 0, Nil), VariableDefinition("ψ", 0, Nil), VariableDefinition("χ", 1, Nil)),
      Seq(VariableDefinition("a", 0, Nil), VariableDefinition("b", 0, Nil))),
    Nil)

  def parseStatement(line: String): Statement = {
    Statement.parser.parseAndDiscard(line)
  }

  def parseStatementList(line: String): Seq[Statement] = {
    Statement.listParser.parseAndDiscard(line)
  }

  "statement parser" should {
    "parse a statement variable" in {
      parseStatement("φ") mustEqual φ.toVariable
    }

    "parse a binary connective" in {
      parseStatement("→ φ ψ") mustEqual Implication(φ, ψ)
    }

    "parse a nested binary connective" in {
      parseStatement("→ → φ ψ φ") mustEqual
        Implication(Implication(φ, ψ), φ)
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
      parseStatementList("(φ)") mustEqual Seq(φ.toVariable)
    }

    "parse a list with multiple statements" in {
      parseStatementList("(φ, ψ, with a χ)") mustEqual Seq(φ(), ψ(), χ(a))
    }

    "parse a type without a qualifier" in {
      parseStatement("is f function") mustEqual Function(f)
    }

    "parse a type with a default qualifier" in {
      parseStatement("is R relation A") mustEqual Relation(R, A)
    }

    "parse a type with an explicit qualifier" in {
      parseStatement("is f function from A B") mustEqual Conjunction(Function(f), FunctionFrom(f, A, B))
    }
  }
}
