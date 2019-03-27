package net.prover.model

import net.prover.model.entries.ExpressionDefinition.{ComponentArgument, PredicateComponent}
import net.prover.model.entries.TermDefinition
import net.prover.model.expressions.{Expression, FunctionParameter}
import org.specs2.execute.Result

class SubstitutionsSpec extends ProverSpec {
  def testSubstitutions(externalDepth: Int, source: Expression, targetExpression: Expression, expectedSubstitutions: Substitutions*) = {
    val calculatedSubstitutions = source.calculateSubstitutions(targetExpression, Substitutions.empty, 0, externalDepth)
    calculatedSubstitutions.must(contain(exactly(expectedSubstitutions: _*)))
    Result.foreach(expectedSubstitutions) { expectedSubstitution =>
      val substitutedExpression = source.applySubstitutions(expectedSubstitution, 0, externalDepth)
      substitutedExpression must beSome(targetExpression)
    }
  }

  "calculating substitutions" should {
    "match the target statement for a statement variable" in {
      testSubstitutions(
        0,
        φ,
        Equals(a, b),
        Substitutions(statements = Map(φ -> Equals(a, b))))
    }

    "match components for a defined statement" in {
      testSubstitutions(
        0,
        Implication(φ, ψ),
        Implication(Equals(a, b), Negation(χ)),
        Substitutions(statements = Map(φ -> Equals(a, b), ψ -> Negation(χ))))
    }

    "not match a defined statement to a statement variable" in {
      testSubstitutions(
        0,
        Implication(φ, ψ),
        χ)
    }

    "not match a defined statement to a different defined statement" in {
      testSubstitutions(
        0,
        Implication(φ, ψ),
        Conjunction(ψ, χ))
    }

    "match two connectives of the same type whose components merge correctly" in {
      testSubstitutions(
        0,
        Implication(φ, φ),
        Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)),
        Substitutions(statements = Map(φ -> Conjunction(φ, ψ))))
    }

    "not match two connectives of the same type whose components do not merge correctly" in {
      testSubstitutions(
        0,
        Implication(φ, φ),
        Implication(Conjunction(φ, ψ), χ))
    }

    "match a predicate application to a defined statement" in {
      testSubstitutions(
        0,
        φ(a),
        ElementOf(b, EmptySet),
        Substitutions(predicates = Map((φ, 1) -> ElementOf(b, EmptySet))),
        Substitutions(terms = Map(a -> b), predicates = Map((φ, 1) -> ElementOf(FunctionParameter(0, 0), EmptySet))),
        Substitutions(terms = Map(a -> EmptySet), predicates = Map((φ, 1) -> ElementOf(b, FunctionParameter(0, 0)))))
    }

    "match a predicate application to another predicate application" in {
      testSubstitutions(
        0,
        φ(a),
        ψ(b),
        Substitutions(predicates = Map((φ, 1) -> ψ(b))),
        Substitutions(terms = Map(a -> b), predicates = Map((φ, 1) -> ψ(FunctionParameter(0, 0)))))
    }

    "match a predicate application to a defined predicate" in {
      testSubstitutions(
        1,
        φ(a),
        ElementOf(FunctionParameter(0, 0), b),
        Substitutions(
          predicates = Map((φ, 1) -> ElementOf(FunctionParameter(0, 0), b))),
        Substitutions(
          terms = Map(a -> FunctionParameter(0, 0)),
          predicates = Map((φ, 1) -> ElementOf(FunctionParameter(0, 1), b))),
        Substitutions(
          terms = Map(a -> b),
          predicates = Map((φ, 1) -> ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))))
    }

    "match a bound predicate application to itself" in {
      testSubstitutions(
        0,
        ForAll("x")(φ(FunctionParameter(0, 0))),
        ForAll("x")(φ(FunctionParameter(0, 0))),
        Substitutions(predicates = Map((φ, 1) -> φ(FunctionParameter(0, 0)))))
    }

    "match a bound connective to itself" in {
      testSubstitutions(
        0,
        ForAll("x")(ElementOf(FunctionParameter(0, 0), a)),
        ForAll("x")(ElementOf(FunctionParameter(0, 0), b)),
        Substitutions(terms = Map(a -> b)))
    }

    "match a bound predicate application to a bound predicate" in {
      testSubstitutions(
        0,
        ForAll("x")(φ(FunctionParameter(0, 0))),
        ForAll("x")(Equals(FunctionParameter(0, 0), a)),
        Substitutions(predicates = Map((φ, 1) -> Equals(FunctionParameter(0, 0), a))))
    }

    "match a predicate application to a higher-order predicate application" in {
      testSubstitutions(
        1,
        φ(a),
        ψ(FunctionParameter(0, 0)),
        Substitutions(
          terms = Map(a -> FunctionParameter(0, 0)),
          predicates = Map((φ, 1) -> ψ(FunctionParameter(0, 1)))),
        Substitutions(
          predicates = Map((φ, 1) -> ψ(FunctionParameter(0, 0)))))
    }

    "match a bound statement to a higher-order bound statement" in {
      testSubstitutions(
        1,
        ForAll("x")(Equals(FunctionParameter(0, 0), FunctionParameter(0, 0))),
        ForAll("x")(Equals(FunctionParameter(0, 0), FunctionParameter(0, 0))),
        Substitutions.empty)
    }

    "match a 1st order bound statement to a 3rd order one" in {
      testSubstitutions(
        2,
        ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), a))),
        ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 2)))),
        Substitutions(terms = Map(a -> FunctionParameter(0, 1)))
      )
    }

    "match a bound predicate application to a 1st-order bound application" in {
      testSubstitutions(
        1,
        ForAll("x")(φ(FunctionParameter(0, 0))),
        ForAll("X")(ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1))))),
        Substitutions(
          predicates = Map((φ, 1) -> ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 2)))))))
    }

    "match a bound predicate application to a 1st-order bound application referencing its external argument" in {
      testSubstitutions(
        1,
        ForAll("x")(φ(FunctionParameter(0, 0))),
        ForAll("X")(ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 2))))),
        Substitutions(
          predicates = Map((φ, 1) -> ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))))))
    }

    "match a predicate application on a variable to a bound statement containing the variable" in {
      testSubstitutions(
        0,
        φ(a),
        Exists("x")(ElementOf(FunctionParameter(0, 0), a)),
        Substitutions(
          terms = Map(a -> a),
          predicates = Map((φ, 1) -> Exists("x")(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1))))),
        Substitutions(
          predicates = Map((φ, 1) -> Exists("x")(ElementOf(FunctionParameter(0, 0), a)))))
    }

    "match a predicate application on a variable to a bound statement containing no variables" in {
      testSubstitutions(
        0,
        φ(a),
        Exists("x")(ElementOf(FunctionParameter(0, 0), EmptySet)),
        Substitutions(
          terms = Map(a -> EmptySet),
          predicates = Map((φ, 1) -> Exists("x")(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1))))),
        Substitutions(
          predicates = Map((φ, 1) -> Exists("x")(ElementOf(FunctionParameter(0, 0), EmptySet)))))
    }

    "match a bound statement variable to a statement" in {
      testSubstitutions(
        0,
        ForAll("x")(Implication(φ(FunctionParameter(0, 0)), ψ)),
        ForAll("x")(Implication(φ(FunctionParameter(0, 0)), Exists("y")(Conjunction(φ(FunctionParameter(0, 0)), ψ)))),
        Substitutions(
          statements = Map(ψ -> Exists("y")(Conjunction(φ(FunctionParameter(0, 0)), ψ))),
          predicates = Map((φ, 1) -> φ(FunctionParameter(0, 0)))))
    }

    "match two bound statements with a large shared context" in {
      testSubstitutions(
        1,
        ForAll("x")(ForAll("y")(φ(FunctionParameter(0, 0)))),
        ForAll("x")(ForAll("y")(Exists("z")(ElementOf(FunctionParameter(0, 1), FunctionParameter(0, 3))))),
        Substitutions(
          predicates = Map((φ, 1) -> Exists("y")(ElementOf(FunctionParameter(0, 2), FunctionParameter(0, 1))))))
    }
  }
}
