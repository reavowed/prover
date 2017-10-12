package net.prover.model

import net.prover.model.expressions.{Expression, FunctionParameter, PredicateApplication}
import org.specs2.execute.Result

class SubstitutionsSpec extends ProverSpec {
  def testSubstitutions(source: Expression, targetExpression: Expression, depth: Int, expectedSubstitutions: Substitutions*) = {
    val calculatedSubstitutions = source.calculateSubstitutions(targetExpression, Substitutions(depth = depth))
    calculatedSubstitutions.must(contain(exactly(expectedSubstitutions: _*)))
    Result.foreach(expectedSubstitutions) { expectedSubstitution =>
      val substitutedExpression = source.applySubstitutions(expectedSubstitution)
      substitutedExpression must beSome(targetExpression)
    }
  }

  "calculating substitutions" should {
    "match the target statement for a statement variable" in {
      testSubstitutions(
        φ,
        Equals(a, b),
        0,
        Substitutions(statements = Map(φ -> Equals(a, b))))
    }

    "match components for a defined statement" in {
      testSubstitutions(
        Implication(φ, ψ),
        Implication(Equals(a, b), Negation(χ)),
        0,
        Substitutions(statements = Map(φ -> Equals(a, b), ψ -> Negation(χ))))
    }

    "not match a defined statement to a statement variable" in {
      testSubstitutions(
        Implication(φ, ψ),
        χ,
        0)
    }

    "not match a defined statement to a different defined statement" in {
      testSubstitutions(
        Implication(φ, ψ),
        Conjunction(ψ, χ),
        0)
    }

    "match two connectives of the same type whose components merge correctly" in {
      testSubstitutions(
        Implication(φ, φ),
        Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)),
        0,
        Substitutions(statements = Map(φ -> Conjunction(φ, ψ))))
    }

    "not match two connectives of the same type whose components do not merge correctly" in {
      testSubstitutions(
        Implication(φ, φ),
        Implication(Conjunction(φ, ψ), χ),
        0)
    }

    "match a predicate application to a defined statement" in {
      testSubstitutions(
        φ(a),
        ElementOf(b, EmptySet),
        0,
        Substitutions(predicates = Map(φ -> ElementOf.!(b.^, EmptySet.^))),
        Substitutions(terms = Map(a -> b), predicates = Map(φ -> ElementOf.!(FunctionParameter.anonymous(0), EmptySet.^))))
    }

    "match a predicate application to another predicate application" in {
      testSubstitutions(
        φ(a),
        ψ(b),
        0,
        Substitutions(predicates = Map(φ -> ψ.!(b.^))),
        Substitutions(terms = Map(a -> b), predicates = Map(φ -> ψ.!(FunctionParameter.anonymous(0)))))
    }

    "match a predicate application to a defined predicate" in {
      testSubstitutions(
        φ(a),
        ElementOf.!(FunctionParameter.anonymous(0), b.^),
        1,
        Substitutions(
          predicates = Map(φ -> ElementOf.!!(FunctionParameter.anonymous(0, 2), b.^^)),
          depth = 1),
        Substitutions(
          terms = Map(a -> FunctionParameter.anonymous(0)),
          predicates = Map(φ -> ElementOf.!!(FunctionParameter.anonymous(0, 1, 2), b.^^)),
          depth = 1),
        Substitutions(
          terms = Map(a -> b.^),
          predicates = Map(φ -> ElementOf.!!(FunctionParameter.anonymous(0, 2), FunctionParameter.anonymous(0, 1, 2))),
          depth = 1))
    }

    "match a higher order predicate application to a predicate" in {
      testSubstitutions(
        φ.!(FunctionParameter("x", 0, 1)),
        ElementOf.!(FunctionParameter("x", 0, 1), a.^),
        0,
        Substitutions(predicates = Map(φ -> ElementOf.!(FunctionParameter("x", 0, 1), a.^))))
    }

    "match a bound predicate application to itself" in {
      testSubstitutions(
        ForAll("x")(φ.!(FunctionParameter("x", 0))),
        ForAll("x")(φ.!(FunctionParameter("x", 0))),
        0,
        Substitutions(predicates = Map(φ -> φ.!(FunctionParameter.anonymous(0)))))
    }

    "match a bound connective to itself" in {
      testSubstitutions(
        ForAll("x")(ElementOf.!(FunctionParameter("x", 0), a.^)),
        ForAll("x")(ElementOf.!(FunctionParameter("x", 0), b.^)),
        0,
        Substitutions(terms = Map(a -> b)))
    }

    "match a bound predicate application to a bound predicate" in {
      testSubstitutions(
        ForAll("x")(φ.!(FunctionParameter("x", 0))),
        ForAll("x")(Equals.!(FunctionParameter("x", 0), FunctionParameter("x", 0))),
        0,
        Substitutions(predicates = Map(φ -> Equals.!(FunctionParameter.anonymous(0), FunctionParameter.anonymous(0)))))
    }

    "not match a predicate application to a bound predicate application" in {
      testSubstitutions(
        φ(a),
        ForAll("x")(φ.!(FunctionParameter("x", 0))),
        0)
    }

    "match a predicate application to a higher-order predicate application" in {
      testSubstitutions(
        φ(a),
        ψ.!(FunctionParameter("x", 0)),
        1,
        Substitutions(
          terms = Map(a -> FunctionParameter("x", 0)),
          predicates = Map(φ -> ψ.!!(FunctionParameter.anonymous(0, 1, 2))),
          depth = 1),
        Substitutions(
          predicates = Map(φ -> ψ.!!(FunctionParameter.anonymous(0, 2))),
          depth = 1))
    }

    "match a bound statement to a higher-order bound statement" in {
      testSubstitutions(
        ForAll("x")(Equals.!(FunctionParameter("x", 0), FunctionParameter("x", 0))),
        ForAll.!("x")(Equals.!!(FunctionParameter("x", 0, 2), FunctionParameter("x", 0, 2))),
        1,
        Substitutions(depth = 1))
    }

    "match a first-order predicate application to a second-order predicate application" in {
      testSubstitutions(
        φ.!(FunctionParameter("x", 0)),
        φ.!!(FunctionParameter("x", 0, 2, 2)),
        1,
        Substitutions(predicates = Map(φ -> φ.!!(FunctionParameter.anonymous(0, 1, 2))), depth = 1))
    }
  }
}
