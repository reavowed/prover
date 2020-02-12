package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{Expression, FunctionParameter}
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class SubstitutionsSpec extends Specification {
  def testSuccessfulMatch(externalDepth: Int, expectedSubstitutions: Substitutions, sourceToTarget: (Expression, Expression)*): Result = {
    val calculatedSubstitutions = sourceToTarget.foldLeft(Option(Substitutions.Possible.empty)) { case (substitutions, (source, target)) =>
      substitutions.flatMap(source.calculateSubstitutions(target, _, 0, externalDepth))
    }.flatMap(_.confirmTotality)
    calculatedSubstitutions must beSome(expectedSubstitutions)
    Result.foreach(sourceToTarget) { case (source, target) =>
      val substitutedExpression = source.applySubstitutions(expectedSubstitutions, 0, externalDepth)
      substitutedExpression must beSome(target)
    }
  }
  def testFailedMatch(externalDepth: Int, sourceToTarget: (Expression, Expression)*): Result = {
    val calculatedSubstitutions = sourceToTarget.foldLeft(Option(Substitutions.Possible.empty)) { case (substitutions, (source, target)) =>
      substitutions.flatMap(source.calculateSubstitutions(target, _, 0, externalDepth))
    }.flatMap(_.confirmTotality)
    calculatedSubstitutions must beNone
  }

  "calculating substitutions" should {
    "match the target statement for a statement variable" in {
      testSuccessfulMatch(
        0,
        Substitutions(statements = Map(φ -> (0, Equals(a, b)))),
        φ -> Equals(a, b))
    }

    "match components for a defined statement" in {
      testSuccessfulMatch(
        0,
        Substitutions(statements = Map(φ -> (0, Equals(a, b)), ψ -> (0, Negation(χ)))),
        Implication(φ, ψ) -> Implication(Equals(a, b), Negation(χ)))
    }

    "not match a defined statement to a statement variable" in {
      testFailedMatch(
        0,
        Implication(φ, ψ) -> χ)
    }

    "not match a defined statement to a different defined statement" in {
      testFailedMatch(
        0,
        Implication(φ, ψ) -> Conjunction(ψ, χ))
    }

    "match two connectives of the same type whose components merge correctly" in {
      testSuccessfulMatch(
        0,
        Substitutions(statements = Map(φ -> (0, Conjunction(φ, ψ)))),
        Implication(φ, φ) -> Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)))
    }

    "not match two connectives of the same type whose components do not merge correctly" in {
      testFailedMatch(
        0,
        Implication(φ, φ) -> Implication(Conjunction(φ, ψ), χ))
    }

    "match a predicate application to a defined statement" in {
      testSuccessfulMatch(
        0,
        Substitutions(
          statements = Map(φ -> (1, ElementOf(b, EmptySet))),
          terms = Map(a -> (0, c))),
        a -> c,
        φ(a) -> ElementOf(b, EmptySet))
      testSuccessfulMatch(
        0,
        Substitutions(terms = Map(a -> (0, b), b -> (0, c)), statements = Map((φ -> (1, ElementOf(FunctionParameter(0, 0), EmptySet))))),
        φ(a) -> ElementOf(b, EmptySet),
        φ(b) -> ElementOf(c, EmptySet))
      testSuccessfulMatch(
        0,
        Substitutions(terms = Map(a -> (0, EmptySet), b -> (0, c)), statements = Map(φ -> (1, ElementOf(b, FunctionParameter(0, 0))))),
        φ(a) -> ElementOf(b, EmptySet),
        φ(b) -> ElementOf(b, c))
      testFailedMatch(
        0,
        φ(a) -> ElementOf(b, EmptySet),
        φ(b) -> ElementOf(c, d))
    }

    "match a predicate application to another predicate application" in {
      testSuccessfulMatch(
        0,
        Substitutions(terms = Map(a -> (0, b), b -> (0, c)), statements = Map(φ -> (1, ψ(FunctionParameter(0, 0))))),
        φ(a) -> ψ(b),
        φ(b) -> ψ(c))
    }

    "match a predicate application to a defined predicate" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, FunctionParameter(0, 0)), b -> (0, c)),
          statements = Map(φ -> (1, ElementOf(FunctionParameter(0, 1), b)))),
        φ(a) -> ElementOf(FunctionParameter(0, 0), b),
        φ(b) -> ElementOf(c, b))
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, b), b -> (0, c)),
          statements = Map(φ -> (1, ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1))))),
        φ(a) -> ElementOf(FunctionParameter(0, 0), b),
        φ(b) -> ElementOf(FunctionParameter(0, 0), c))
    }

    "match a bound predicate application to itself" in {
      testSuccessfulMatch(
        0,
        Substitutions(statements = Map(φ -> (1, φ(FunctionParameter(0, 0))))),
        ForAll("x")(φ(FunctionParameter(0, 0)))-> ForAll("x")(φ(FunctionParameter(0, 0))))
    }

    "match a bound connective to itself" in {
      testSuccessfulMatch(
        0,
        Substitutions(terms = Map(a -> (0, b))),
        ForAll("x")(ElementOf(FunctionParameter(0, 0), a)) -> ForAll("x")(ElementOf(FunctionParameter(0, 0), b)))
    }

    "match a bound predicate application to a bound predicate" in {
      testSuccessfulMatch(
        0,
        Substitutions(statements = Map(φ -> (1, Equals(FunctionParameter(0, 0), a)))),
        ForAll("x")(φ(FunctionParameter(0, 0)))-> ForAll("x")(Equals(FunctionParameter(0, 0), a)))
    }

    "match a predicate application to a higher-order predicate application" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, FunctionParameter(0, 0)), b -> (0, c)),
          statements = Map(φ -> (1, ψ(FunctionParameter(0, 1))))),
        φ(a) -> ψ(FunctionParameter(0, 0)),
        φ(b) -> ψ(c))
    }

    "match a bound statement to a higher-order bound statement" in {
      testSuccessfulMatch(
        1,
        Substitutions.empty,
        ForAll("x")(Equals(FunctionParameter(0, 0), FunctionParameter(0, 0))) -> ForAll("x")(Equals(FunctionParameter(0, 0), FunctionParameter(0, 0))))
    }

    "match a 1st order bound statement to a 3rd order one" in {
      testSuccessfulMatch(
        2,
        Substitutions(terms = Map(a -> (0, FunctionParameter(0, 1)))),
        ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), a))) -> ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 2)))))
    }

    "match a bound predicate application to a 1st-order bound application" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          statements = Map(φ -> (1, ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 2))))))),
        ForAll("x")(φ(FunctionParameter(0, 0))) -> ForAll("y")(ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1))))))
    }

    "match a bound predicate application to a 1st-order bound application referencing its external argument" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          statements = Map(φ -> (1, ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1))))))),
        ForAll("x")(φ(FunctionParameter(0, 0)))-> ForAll("y")(ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 2))))))
    }

    "match a predicate application on a variable to a bound statement containing the variable" in {
      testSuccessfulMatch(
        0,
        Substitutions(
          terms = Map(a -> (0, a), b -> (0, c)),
          statements = Map(φ -> (1, Exists("x")(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))))),
        φ(a) -> Exists("x")(ElementOf(FunctionParameter(0, 0), a)),
        φ(b) -> Exists("x")(ElementOf(FunctionParameter(0, 0), c)))
    }

    "match a predicate application on a variable to a bound statement containing no variables" in {
      testSuccessfulMatch(
        0,
        Substitutions(
          terms = Map(a -> (0, EmptySet), b -> (0, c)),
          statements = Map(φ -> (1, Exists("x")(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1)))))),
        φ(a) -> Exists("x")(ElementOf(FunctionParameter(0, 0), EmptySet)),
        φ(b) -> Exists("x")(ElementOf(FunctionParameter(0, 0), c)))
    }

    "match a bound statement variable to a statement" in {
      testSuccessfulMatch(
        0,
        Substitutions(
          statements = Map(
            ψ -> (0, Exists("y")(Conjunction(φ(FunctionParameter(0, 0)), ψ))),
            φ -> (1, φ(FunctionParameter(0, 0))))),
        ForAll("x")(Implication(φ(FunctionParameter(0, 0)), ψ)) -> ForAll("x")(Implication(φ(FunctionParameter(0, 0)), Exists("y")(Conjunction(φ(FunctionParameter(0, 0)), ψ)))))
    }

    "match two bound statements with a large shared context" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          statements = Map(φ -> (1, Exists("y")(ElementOf(FunctionParameter(0, 2), FunctionParameter(0, 1)))))),
        ForAll("x")(ForAll("y")(φ(FunctionParameter(0, 0)))) -> ForAll("x")(ForAll("y")(Exists("z")(ElementOf(FunctionParameter(0, 1), FunctionParameter(0, 3))))))
    }

    "match nested applications" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, c), F -> (1, b)),
          statements = Map(φ -> (1, Equals(FunctionParameter(0, 1), FunctionParameter(0, 0))))),
        φ(F(a)) -> Equals(b, FunctionParameter(0, 0)),
        φ(F(b)) -> Equals(b, FunctionParameter(0, 0)),
        φ(a) -> Equals(c, FunctionParameter(0, 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, b), b -> (0, c), F -> (1, FunctionParameter(0, 1))),
          statements = Map(φ -> (1, Equals(FunctionParameter(0, 1), FunctionParameter(0, 0))))),
        φ(F(a)) -> Equals(b, FunctionParameter(0, 0)),
        φ(F(b)) -> Equals(c, FunctionParameter(0, 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, c), F -> (1, FunctionParameter(0, 0))),
          statements = Map(φ -> (1, Equals(b, FunctionParameter(0, 1))))),
        φ(F(a)) -> Equals(b, FunctionParameter(0, 0)),
        φ(F(b)) -> Equals(b, FunctionParameter(0, 0)),
        φ(a) -> Equals(b, c))
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, FunctionParameter(0, 0)), b -> (0, c), F -> (1, FunctionParameter(0, 1))),
          statements = Map(φ -> (1, Equals(b, FunctionParameter(0, 1))))),
        φ(F(a)) -> Equals(b, FunctionParameter(0, 0)),
        φ(F(b)) -> Equals(b, c))
    }

    "match bound nested applications" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, c), F -> (1, b)),
          statements = Map(φ -> (1, Equals(FunctionParameter(0, 1), FunctionParameter(0, 0))))),
        ForAll("x")(φ(F(a))) -> ForAll("x")(Equals(b, FunctionParameter(0, 1))),
        ForAll("x")(φ(a)) -> ForAll("x")(Equals(c, FunctionParameter(0, 1))))
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, b), b -> (0, c), F -> (1, FunctionParameter(0, 1))),
          statements = Map(φ -> (1, Equals(FunctionParameter(0, 1), FunctionParameter(0, 0))))),
        ForAll("x")(φ(F(a))) -> ForAll("x")(Equals(b, FunctionParameter(0, 1))),
        ForAll("x")(φ(F(b))) -> ForAll("x")(Equals(c, FunctionParameter(0, 1))))
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, b), F -> (1, FunctionParameter(0, 0))),
          statements = Map(φ -> (1, Equals(b, FunctionParameter(0, 1))))),
        ForAll("x")(φ(F(a))) -> ForAll("x")(Equals(b, FunctionParameter(0, 1))),
        ForAll("x")(φ(F(b))) -> ForAll("x")(Equals(b, FunctionParameter(0, 1))),
        ForAll("x")(φ(a)) -> ForAll("x")(Equals(b, b)))
      testSuccessfulMatch(
        1,
        Substitutions(
          terms = Map(a -> (0, FunctionParameter(0, 0)), b -> (0, c), F -> (1, FunctionParameter(0, 1))),
          statements = Map(φ -> (1, Equals(b, FunctionParameter(0, 1))))),
        ForAll("x")(φ(F(a))) -> ForAll("x")(Equals(b, FunctionParameter(0, 1))),
        ForAll("x")(φ(F(b))) -> ForAll("x")(Equals(b, c)),
        ForAll("x")(φ(b)) -> ForAll("x")(Equals(b, c)))
    }

    "match more bound nested applications" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          statements = Map(φ -> (1, ElementOf(Singleton(FunctionParameter(0, 1)), FunctionParameter(0, 0)))),
          terms = Map(F -> (1, FunctionParameter(0, 1)))),
        ForAll("x")(φ(F(FunctionParameter(0, 0)))) -> ForAll("x")(ElementOf(Singleton(FunctionParameter(0, 0)), FunctionParameter(0, 1))),
        F(FunctionParameter(0, 0)) -> FunctionParameter(0, 0))
      testSuccessfulMatch(
        1,
        Substitutions(
          statements = Map(φ -> (1, ElementOf(FunctionParameter(0, 1), FunctionParameter(0, 0)))),
          terms = Map(F -> (1, Singleton(FunctionParameter(0, 1))))),
        ForAll("x")(φ(F(FunctionParameter(0, 0)))) -> ForAll("x")(ElementOf(Singleton(FunctionParameter(0, 0)), FunctionParameter(0, 1))),
        F(FunctionParameter(0, 0)) -> Singleton(FunctionParameter(0, 0)))
    }

    "something something" in {
      ForAll("x")(φ(F(a)))
        .applySubstitutions(
          Substitutions(
            statements = Map(φ -> (1, Equals(b, FunctionParameter(0, 1)))),
            terms = Map(F -> (1, FunctionParameter(0, 0)))),
          0,
          1)
        .mustEqual(Some(ForAll("x")(Equals(b, FunctionParameter(0, 1)))))
    }

    "match a variable to a parameter at different internal depths" in {
      testSuccessfulMatch(
        1,
        Substitutions(terms = Map(a -> (0, $), b -> (0, c))),
        Conjunction(Equals(a, b), ForAll("x")(ElementOf(a, $))) -> Conjunction(Equals($, c), ForAll("x")(ElementOf($.^, $))))
    }
  }
}
