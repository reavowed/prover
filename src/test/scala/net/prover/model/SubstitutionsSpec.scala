package net.prover.model

import net.prover.model.expressions.{ArgumentList, Expression, FunctionParameter}
import org.specs2.execute.Result

class SubstitutionsSpec extends ProverSpec {
  def testSubstitutions(source: Expression, targetExpression: Expression, rawSubstitutions: Substitutions*) = {
    val substitutionsDepth = targetExpression.depth - source.depth
    val expectedSubstitutions = rawSubstitutions.map(_.copy(depth = substitutionsDepth))
    val calculatedSubstitutions = source.calculateSubstitutions(targetExpression, Substitutions(depth = substitutionsDepth), Nil, Nil)
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
        Substitutions(statements = Map(φ -> Equals(a, b))))
    }

    "match components for a defined statement" in {
      testSubstitutions(
        Implication(φ, ψ),
        Implication(Equals(a, b), Negation(χ)),
        Substitutions(statements = Map(φ -> Equals(a, b), ψ -> Negation(χ))))
    }

    "not match a defined statement to a statement variable" in {
      testSubstitutions(
        Implication(φ, ψ),
        χ)
    }

    "not match a defined statement to a different defined statement" in {
      testSubstitutions(
        Implication(φ, ψ),
        Conjunction(ψ, χ))
    }

    "match two connectives of the same type whose components merge correctly" in {
      testSubstitutions(
        Implication(φ, φ),
        Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)),
        Substitutions(statements = Map(φ -> Conjunction(φ, ψ))))
    }

    "not match two connectives of the same type whose components do not merge correctly" in {
      testSubstitutions(
        Implication(φ, φ),
        Implication(Conjunction(φ, ψ), χ))
    }

    "match a predicate application to a defined statement" in {
      testSubstitutions(
        φ(a),
        ElementOf(b, EmptySet),
        Substitutions(predicates = Map(φ -> ElementOf.!(b.^, EmptySet.^))),
        Substitutions(terms = Map(a -> b), predicates = Map(φ -> ElementOf.!(FunctionParameter.anonymous(0), EmptySet.^))),
        Substitutions(terms = Map(a -> EmptySet), predicates = Map(φ -> ElementOf.!(b.^, FunctionParameter.anonymous(0)))))
    }

    "match a predicate application to another predicate application" in {
      testSubstitutions(
        φ(a),
        ψ(b),
        Substitutions(predicates = Map(φ -> ψ.!(b.^))),
        Substitutions(terms = Map(a -> b), predicates = Map(φ -> ψ.!(FunctionParameter.anonymous(0)))))
    }

    "match a predicate application to a defined predicate" in {
      testSubstitutions(
        φ(a),
        ElementOf.!(FunctionParameter.anonymous(0), b.^),
        Substitutions(
          predicates = Map(φ -> ElementOf.!!(FunctionParameter.anonymous(0, 2), b.^^))),
        Substitutions(
          terms = Map(a -> FunctionParameter.anonymous(0)),
          predicates = Map(φ -> ElementOf.!!(FunctionParameter.anonymous(0, 1, 2), b.^^))),
        Substitutions(
          terms = Map(a -> b.^),
          predicates = Map(φ -> ElementOf.!!(FunctionParameter.anonymous(0, 2), FunctionParameter.anonymous(0, 1, 2)))))
    }

    "match a 1st order predicate application to a 1st order predicate" in {
      testSubstitutions(
        φ.!(FunctionParameter("x", 0, 1)),
        ElementOf.!(FunctionParameter("x", 0, 1), a.^),
        Substitutions(predicates = Map(φ -> ElementOf.!(FunctionParameter.anonymous(0, 1), a.^))))
    }

    "match a bound predicate application to itself" in {
      testSubstitutions(
        ForAll("x")(φ.!(FunctionParameter("x", 0))),
        ForAll("x")(φ.!(FunctionParameter("x", 0))),
        Substitutions(predicates = Map(φ -> φ.!(FunctionParameter.anonymous(0)))))
    }

    "match a bound connective to itself" in {
      testSubstitutions(
        ForAll("x")(ElementOf.!(FunctionParameter("x", 0), a.^)),
        ForAll("x")(ElementOf.!(FunctionParameter("x", 0), b.^)),
        Substitutions(terms = Map(a -> b)))
    }

    "match a bound predicate application to a bound predicate" in {
      testSubstitutions(
        ForAll("x")(φ.!(FunctionParameter("x", 0))),
        ForAll("x")(Equals.!(FunctionParameter("x", 0), FunctionParameter("x", 0))),
        Substitutions(predicates = Map(φ -> Equals.!(FunctionParameter.anonymous(0), FunctionParameter.anonymous(0)))))
    }

    "match a predicate application to a higher-order predicate application" in {
      testSubstitutions(
        φ(a),
        ψ.!(FunctionParameter("x", 0)),
        Substitutions(
          terms = Map(a -> FunctionParameter("x", 0)),
          predicates = Map(φ -> ψ.!!(FunctionParameter.anonymous(0, 1, 2)))),
        Substitutions(
          predicates = Map(φ -> ψ.!!(FunctionParameter.anonymous(0, 2)))))
    }

    "match a bound statement to a higher-order bound statement" in {
      testSubstitutions(
        ForAll("x")(Equals.!(FunctionParameter("x", 0), FunctionParameter("x", 0))),
        ForAll.!("x")(Equals.!!(FunctionParameter("x", 0, 2), FunctionParameter("x", 0, 2))),
        Substitutions.empty)
    }

    "match a first-order predicate application to a second-order predicate application" in {
      testSubstitutions(
        φ.!(FunctionParameter("x", 0)),
        φ.!!(FunctionParameter("x", 0, 2, 2)),
        Substitutions(predicates = Map(φ -> φ.!!(FunctionParameter.anonymous(0, 1, 2)))))
    }

    "match a 1st order bound statement to a 3rd order one" in {
      testSubstitutions(
        ForAll("x")(Negation.!(ElementOf.!(FunctionParameter("x", 0, 1, 1), a.^))),
        ForAll.!!("x")(Negation.!!!(ElementOf.!!!(FunctionParameter("x", 0, 3, 3), FunctionParameter("X", 0, 1, 3)))),
        Substitutions(terms = Map(a -> FunctionParameter("X", 0, 1, 2)))
      )
    }

    "match a bound predicate application to a 1st-order bound application" in {
      testSubstitutions(
        ForAll("x")(φ.!(FunctionParameter("x", 0, 1, 1))),
        ForAll.!("X")(ForAll.!!("x")(Negation.!!!(ElementOf.!!!(FunctionParameter("x", 0, 3, 3), FunctionParameter("X", 0, 2, 3))))),
        Substitutions(
          predicates = Map(φ -> ForAll.!!("x")(Negation.!!!(ElementOf.!!!(FunctionParameter("x", 0, 3, 3), FunctionParameter.anonymous(0, 1, 3)))))))
    }

    "match a bound predicate application to a 1st-order bound application referencing its external argument" in {
      testSubstitutions(
        ForAll("x")(φ.!(FunctionParameter("x", 0, 1, 1))),
        ForAll.!("X")(ForAll.!!("x")(Negation.!!!(ElementOf.!!!(FunctionParameter("x", 0, 3, 3), FunctionParameter("Y", 0, 1, 3))))),
        Substitutions(
          predicates = Map(φ -> ForAll.!!("x")(Negation.!!!(ElementOf.!!!(FunctionParameter("x", 0, 3, 3), FunctionParameter("Y", 0, 2, 3)))))))
    }

    "match a predicate application on a variable to a bound statement containing the variable" in {
      testSubstitutions(
        φ(a),
        Exists("x")(ElementOf.!(FunctionParameter("x", 0, 1), a.^)),
        Substitutions(
          terms = Map(a -> a),
          predicates = Map(φ -> Exists.!("x")(ElementOf.!!(FunctionParameter("x", 0, 2), FunctionParameter.anonymous(0, 1, 2))))),
        Substitutions(
          predicates = Map(φ -> Exists.!("x")(ElementOf.!!(FunctionParameter("x", 0, 2), a.^^)))))
    }

    "match a predicate application on a variable to a bound statement containing no variables" in {
      testSubstitutions(
        φ(a),
        Exists("x")(ElementOf.!(FunctionParameter("x", 0, 1), EmptySet.^)),
        Substitutions(
          terms = Map(a -> EmptySet),
          predicates = Map(φ -> Exists.!("x")(ElementOf.!!(FunctionParameter("x", 0, 2), FunctionParameter.anonymous(0, 1, 2))))),
        Substitutions(
          predicates = Map(φ -> Exists.!("x")(ElementOf.!!(FunctionParameter("x", 0, 2), EmptySet.^^)))))
    }

    "match a bound statement variable to a statement" in {
      testSubstitutions(
        ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.^)),
        ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), Exists.!("y")(Conjunction.!!(φ.!!(FunctionParameter("y", 0, 2)), ψ.^^)))),
        Substitutions(
          statements = Map(ψ -> Exists("y")(Conjunction.!(φ.!(FunctionParameter("y", 0)), ψ.^))),
          predicates = Map(φ -> φ.!(FunctionParameter("x", 0)))))
    }
  }

  "validating hints" should {
    "allow applicative hints to have arguments in external bound variables" in {
      φ.calculateSubstitutions(
        ElementOf.!(FunctionParameter("x", 0), a.^),
        Substitutions.emptyWithDepth(1),
        Seq(Substitutions(statements = Map(φ -> ElementOf.!!(FunctionParameter("x", 0, 1, 2), a.^^)), depth = 2) -> ArgumentList(Seq(a), 0)),
        Nil
      ) mustEqual Seq(Substitutions(statements = Map(φ -> ElementOf.!(FunctionParameter("x", 0), a.^)), depth = 1))
    }
  }
}
