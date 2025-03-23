package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.expressions._
import org.specs2.mutable.Specification

class StatementSpec extends Specification {
  given availableEntries: AvailableEntries = defaultAvailableEntries
  implicit def tupleToVariableDefinition(tuple: (String, Int)): VariableDefinition = VariableDefinition(tuple._1, tuple._2, Nil)

  def parseStatement(line: String)(implicit variableDefinitions: VariableDefinitions): Statement = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)
    Statement.parser.parseAndDiscard(line)
  }

  def parseStatementList(line: String)(implicit variableDefinitions: VariableDefinitions): Seq[Statement] = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)
    Statement.listParser.parseAndDiscard(line)
  }

  "statement parser" should {
    "parse a statement variable" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 0), Nil)
      parseStatement("φ") must beEqualTo(φ.toVariable)
    }

    "parse a binary connective" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Nil)
      parseStatement("→ φ ψ") must beEqualTo(Implication(φ, ψ))
    }

    "parse a nested binary connective" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Nil)
      parseStatement("→ → φ ψ φ") must beEqualTo(Implication(Implication(φ, ψ), φ))
    }

    "parse a bound predicate application" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1), Nil)
      parseStatement("∀ y with y φ") must beEqualTo(ForAll("y")(φ($)))
    }

    "parse a bound defined statement" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0))
      parseStatement("∀ y ↔ ∈ y a ∈ y b") must beEqualTo(ForAll("y")(Equivalence(
        ElementOf(FunctionParameter(0, 0), a),
        ElementOf(FunctionParameter(0, 0), b))))
    }

    "parse an empty list" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Nil)
      parseStatementList("()") must beEqualTo(Nil)
    }

    "parse a list with a single statement" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 0), Nil)
      parseStatementList("(φ)") must beEqualTo(Seq(φ.toVariable))
    }

    "parse a list with multiple statements" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0, χ -> 1), Seq(a -> 0))
      parseStatementList("(φ, ψ, with a χ)") must beEqualTo(Seq(φ(), ψ(), χ(a)))
    }

    "parse a type without a qualifier" in {
      val f = TermVariablePlaceholder("f", 0)
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(f -> 0))
      parseStatement("is f function") must beEqualTo(Function(f))
    }

    "parse a type with a default qualifier" in {
      val R = TermVariablePlaceholder("R", 0)
      val A = TermVariablePlaceholder("A", 1)
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(R -> 0, A -> 0))
      parseStatement("is R relation A") must beEqualTo(Relation(R, A))
    }

    "parse a type with an explicit qualifier" in {
      val f = TermVariablePlaceholder("f", 0)
      val A = TermVariablePlaceholder("A", 1)
      val B = TermVariablePlaceholder("B", 2)
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(f -> 0, A -> 0, B -> 0))
      parseStatement("is f function from A B") must beEqualTo(Conjunction(Function(f), FunctionFrom(f, A, B)))
    }
  }
}
