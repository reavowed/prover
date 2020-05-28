package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.expressions._
import org.specs2.mutable.Specification

class StatementSpec extends Specification {
  implicit def tupleToVariableDefinition(tuple: (String, Int)): VariableDefinition = VariableDefinition(tuple._1, tuple._2, Nil)

  def parseStatement(line: String, statements: Seq[VariableDefinition], terms: Seq[VariableDefinition]): Statement = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.withDefinitions(VariableDefinitions(statements, terms))
    Statement.parser.parseAndDiscard(line)
  }

  def parseStatementList(line: String, statements: Seq[VariableDefinition], terms: Seq[VariableDefinition]): Seq[Statement] = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.withDefinitions(VariableDefinitions(statements, terms))
    Statement.listParser.parseAndDiscard(line)
  }

  "statement parser" should {
    "parse a statement variable" in {
      parseStatement("φ", Seq("φ" -> 0), Nil) mustEqual φ.toVariable
    }

    "parse a binary connective" in {
      parseStatement("→ φ ψ", Seq("φ" -> 0, "ψ" -> 0), Nil) mustEqual Implication(φ, ψ)
    }

    "parse a nested binary connective" in {
      parseStatement("→ → φ ψ φ", Seq("φ" -> 0, "ψ" -> 0), Nil) mustEqual
        Implication(Implication(φ, ψ), φ)
    }

    "parse a bound predicate application" in {
      parseStatement("∀ y with y φ", Seq("φ" -> 1), Nil) mustEqual ForAll("y")(φ(FunctionParameter(0, 0)))
    }

    "parse a bound defined statement" in {
      parseStatement("∀ y ↔ ∈ y a ∈ y b", Nil, Seq("a" -> 0, "b" -> 0)) mustEqual ForAll("y")(Equivalence(
        ElementOf(FunctionParameter(0, 0), a),
        ElementOf(FunctionParameter(0, 0), b)))
    }

    "parse an empty list" in {
      parseStatementList("()", Nil, Nil) mustEqual Nil
    }

    "parse a list with a single statement" in {
      parseStatementList("(φ)", Seq("φ" -> 0), Nil) mustEqual Seq(φ.toVariable)
    }

    "parse a list with multiple statements" in {
      parseStatementList("(φ, ψ, with a χ)", Seq("φ" -> 0, "ψ" -> 0, "χ" -> 1), Seq("a" -> 0)) mustEqual Seq(φ(), ψ(), χ(a))
    }

    "parse a type without a qualifier" in {
      parseStatement("is f function", Nil, Seq("f" -> 0)) mustEqual Function(f)
    }

    "parse a type with a default qualifier" in {
      parseStatement("is R relation A", Nil, Seq("R" -> 0, "A" -> 0)) mustEqual Relation(R, A)
    }

    "parse a type with an explicit qualifier" in {
      parseStatement("is f function from A B", Nil, Seq("f" -> 0, "A" -> 0, "B" -> 0)) mustEqual Conjunction(Function(f), FunctionFrom(f, A, B))
    }
  }
}
