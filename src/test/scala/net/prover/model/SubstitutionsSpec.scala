package net.prover.model

import net.prover.core.transformers.ContextWithExternalDepth
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{Expression, FunctionParameter}
import net.prover.old.OldSubstitutionApplier
import net.prover.substitutionFinding.model.PossibleSubstitutions
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class SubstitutionsSpec extends Specification {
  def testSuccessfulMatch(externalDepth: Int, expectedSubstitutions: Substitutions, sourceToTarget: (Expression, Expression)*)(implicit variableDefinitions: VariableDefinitions): Result = {
    val calculatedSubstitutions = sourceToTarget.foldLeft(Option(PossibleSubstitutions.empty)) { case (substitutions, (source, target)) =>
      substitutions.flatMap(PossibleSubstitutionCalculator.calculatePossibleSubstitutions(source, target, _)(ContextWithExternalDepth(externalDepth)))
    }.flatMap(_.confirmTotality(variableDefinitions))
    calculatedSubstitutions must beSome(expectedSubstitutions)
    Result.foreach(sourceToTarget) { case (source, target) =>
      val substitutedExpression = OldSubstitutionApplier.applySubstitutions(source, expectedSubstitutions)(ContextWithExternalDepth(externalDepth))
      substitutedExpression must beSuccessfulTry(target)
    }
  }
  def testFailedMatch(externalDepth: Int, sourceToTarget: (Expression, Expression)*)(implicit variableDefinitions: VariableDefinitions): Result = {
    val calculatedSubstitutions = sourceToTarget.foldLeft(Option(PossibleSubstitutions.empty)) { case (substitutions, (source, target)) =>
      substitutions.flatMap(PossibleSubstitutionCalculator.calculatePossibleSubstitutions(source, target, _)(ContextWithExternalDepth(externalDepth)))
    }.flatMap(_.confirmTotality(variableDefinitions))
    calculatedSubstitutions must beNone
  }

  "calculating substitutions" should {
    "match the target statement for a statement variable" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0), Nil)
      testSuccessfulMatch(
        0,
        Substitutions(Seq(Equals(a, b)), Nil),
        φ -> Equals(a, b))
    }

    "match components for a defined statement" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Nil)
      testSuccessfulMatch(
        0,
        Substitutions(Seq(Equals(a, b), Negation(χ)), Nil),
        Implication(φ, ψ) -> Implication(Equals(a, b), Negation(χ)))
    }

    "not match a defined statement to a statement variable" in {
      testFailedMatch(
        0,
        Implication(φ, ψ) -> χ)(
        getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Nil))
    }

    "not match a defined statement to a different defined statement" in {
      testFailedMatch(
        0,
        Implication(φ, ψ) -> Conjunction(ψ, χ))(
        getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Nil))
    }

    "match two connectives of the same type whose components merge correctly" in {
      testSuccessfulMatch(
        0,
        Substitutions(Seq(Conjunction(φ, ψ)), Nil),
        Implication(φ, φ) -> Implication(Conjunction(φ, ψ), Conjunction(φ, ψ)))(
        getVariableDefinitions(Seq(φ -> 0), Nil))
    }

    "not match two connectives of the same type whose components do not merge correctly" in {
      testFailedMatch(
        0,
        Implication(φ, φ) -> Implication(Conjunction(φ, ψ), χ))(
        getVariableDefinitions(Seq(φ -> 0), Nil))
    }

    "match a predicate application to a defined statement" in {
      testSuccessfulMatch(
        0,
        Substitutions(
          Seq(ElementOf(b, EmptySet)),
          Seq(c)),
        a -> c,
        φ(a) -> ElementOf(b, EmptySet))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0)))
      testSuccessfulMatch(
        0,
        Substitutions(
          Seq(ElementOf($, EmptySet)),
          Seq(b, c)),
        φ(a) -> ElementOf(b, EmptySet),
        φ(b) -> ElementOf(c, EmptySet))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
      testSuccessfulMatch(
        0,
        Substitutions(
          Seq(ElementOf(b, $)),
          Seq(EmptySet, c)),
        φ(a) -> ElementOf(b, EmptySet),
        φ(b) -> ElementOf(b, c))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
      testFailedMatch(
        0,
        φ(a) -> ElementOf(b, EmptySet),
        φ(b) -> ElementOf(c, d))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
    }

    "match a predicate application to another predicate application" in {
      testSuccessfulMatch(
        0,
        Substitutions(
          Seq(ψ($)),
          Seq(b, c)),
        φ(a) -> ψ(b),
        φ(b) -> ψ(c))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
    }

    "match a predicate application to a defined predicate" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(ElementOf($.^, b)),
          Seq($, c)),
        φ(a) -> ElementOf(FunctionParameter(0, 0), b),
        φ(b) -> ElementOf(c, b))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(ElementOf($, $.^)),
          Seq(b, c)),
        φ(a) -> ElementOf($, b),
        φ(b) -> ElementOf($, c))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
    }

    "match a bound predicate application to itself" in {
      testSuccessfulMatch(
        0,
        Substitutions(Seq(φ($)), Nil),
        ForAll("x")(φ($))-> ForAll("x")(φ($)))(
        getVariableDefinitions(Seq(φ -> 1), Nil))
    }

    "match a bound connective to itself" in {
      testSuccessfulMatch(
        0,
        Substitutions(Nil, Seq(b)),
        ForAll("x")(ElementOf($, a)) -> ForAll("x")(ElementOf($, b)))(
        getVariableDefinitions(Nil, Seq(a -> 0)))
    }

    "match a bound predicate application to a bound predicate" in {
      testSuccessfulMatch(
        0,
        Substitutions(Seq(Equals($, a)), Nil),
        ForAll("x")(φ($))-> ForAll("x")(Equals($, a)))(
        getVariableDefinitions(Seq(φ -> 1), Nil))
    }

    "match a predicate application to a higher-order predicate application" in {
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(ψ($.^)),
          Seq($, c)),
        φ(a) -> ψ($),
        φ(b) -> ψ(c))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
    }

    "match a bound statement to a higher-order bound statement" in {
      testSuccessfulMatch(
        1,
        Substitutions.empty,
        ForAll("x")(Equals($, $)) -> ForAll("x")(Equals($, $)))(
        VariableDefinitions.empty)
    }

    "match a 1st order bound statement to a 3rd order one" in {
      testSuccessfulMatch(
        2,
        Substitutions(Nil, Seq($.^)),
        ForAll("x")(Negation(ElementOf($, a))) -> ForAll("x")(Negation(ElementOf($, $.^^))))(
        getVariableDefinitions(Nil, Seq(a -> 0)))
    }

    "match a bound predicate application to a 1st-order bound application" in {
      testSuccessfulMatch(
        1,
        Substitutions(Seq(ForAll("x")(Negation(ElementOf($, $.^^)))), Nil),
        ForAll("x")(φ($)) -> ForAll("y")(ForAll("x")(Negation(ElementOf($, $.^)))))(
        getVariableDefinitions(Seq(φ -> 1), Nil))
    }

    "match a bound predicate application to a 1st-order bound application referencing its external argument" in {
      testSuccessfulMatch(
        1,
        Substitutions(Seq(ForAll("x")(Negation(ElementOf(FunctionParameter(0, 0), FunctionParameter(0, 1))))), Nil),
        ForAll("x")(φ($))-> ForAll("y")(ForAll("x")(Negation(ElementOf($, $.^^)))))(
        getVariableDefinitions(Seq(φ -> 1), Nil))
    }

    "match a predicate application on a variable to a bound statement containing the variable" in {
      testSuccessfulMatch(
        0,
        Substitutions(
          Seq(Exists("x")(ElementOf($, $.^))),
          Seq(a, c)),
        φ(a) -> Exists("x")(ElementOf($, a)),
        φ(b) -> Exists("x")(ElementOf($, c)))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
    }

    "match a predicate application on a variable to a bound statement containing no variables" in {
      testSuccessfulMatch(
        0,
        Substitutions(
          Seq(Exists("x")(ElementOf($, $.^))),
          Seq(EmptySet, c)),
        φ(a) -> Exists("x")(ElementOf($, EmptySet)),
        φ(b) -> Exists("x")(ElementOf($, c)))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
    }

    "match a bound statement variable to a statement" in {
      testSuccessfulMatch(
        0,
        Substitutions(Seq(φ($), Exists("y")(Conjunction(φ($), ψ))), Nil),
        ForAll("x")(Implication(φ($), ψ)) -> ForAll("x")(Implication(φ($), Exists("y")(Conjunction(φ($), ψ)))))(
        getVariableDefinitions(Seq(φ -> 1, ψ -> 0), Nil))
    }

    "match two bound statements with a large shared context" in {
      testSuccessfulMatch(
        1,
        Substitutions(Seq(Exists("y")(ElementOf($.^^, $.^))), Nil),
        ForAll("x")(ForAll("y")(φ($))) -> ForAll("x")(ForAll("y")(Exists("z")(ElementOf($.^, $.^^^)))))(
        getVariableDefinitions(Seq(φ -> 1), Nil))
    }

    "match nested applications" in {
      val F = TermVariablePlaceholder("F", 2)
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(Equals($.^, $)),
          Seq(c, c, b)),
        φ(F(a)) -> Equals(b, $),
        φ(F(b)) -> Equals(b, $),
        φ(a) -> Equals(c, $),
        φ(b) -> Equals(c, $))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, F -> 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(Equals($.^, $)),
          Seq(b, c, $.^)),
        φ(F(a)) -> Equals(b, $),
        φ(F(b)) -> Equals(c, $))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, F -> 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(Equals(b, $.^)),
          Seq(c, c, $)),
        φ(F(a)) -> Equals(b, $),
        φ(F(b)) -> Equals(b, $),
        φ(a) -> Equals(b, c),
        φ(b) -> Equals(b, c))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, F -> 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(Equals(b, $.^)),
          Seq($, c, $.^)),
        φ(F(a)) -> Equals(b, $),
        φ(F(b)) -> Equals(b, c))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, F -> 0)))
    }

    "match bound nested applications" in {
      val F = TermVariablePlaceholder("F", 2)
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(Equals($.^, $)),
          Seq(c, c, b)),
        ForAll("x")(φ(F(a))) -> ForAll("x")(Equals(b, $.^)),
        ForAll("x")(φ(a)) -> ForAll("x")(Equals(c, $.^)),
        ForAll("x")(φ(b)) -> ForAll("x")(Equals(c, $.^)))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, F -> 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(Equals($.^, $)),
          Seq(b, c, $.^)),
        ForAll("x")(φ(F(a))) -> ForAll("x")(Equals(b, $.^)),
        ForAll("x")(φ(F(b))) -> ForAll("x")(Equals(c, $.^)))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, F -> 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(Equals(b, $.^)),
          Seq(b, b, $)),
        ForAll("x")(φ(F(a))) -> ForAll("x")(Equals(b, $.^)),
        ForAll("x")(φ(F(b))) -> ForAll("x")(Equals(b, $.^)),
        ForAll("x")(φ(a)) -> ForAll("x")(Equals(b, b)),
        ForAll("x")(φ(b)) -> ForAll("x")(Equals(b, b)))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, F -> 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(Equals(b, $.^)),
          Seq($, c, $.^)),
        ForAll("x")(φ(F(a))) -> ForAll("x")(Equals(b, $.^)),
        ForAll("x")(φ(F(b))) -> ForAll("x")(Equals(b, c)),
        ForAll("x")(φ(b)) -> ForAll("x")(Equals(b, c)))(
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, F -> 0)))
    }

    "match more bound nested applications" in {
      val F = TermVariablePlaceholder("F", 0)
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(ElementOf(Singleton($.^), $)),
          Seq($.^)),
        ForAll("x")(φ(F($))) -> ForAll("x")(ElementOf(Singleton($), $.^)),
        F($) -> $)(
        getVariableDefinitions(Seq(φ -> 1), Seq(F -> 0)))
      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(ElementOf($.^, $)),
          Seq(Singleton($.^))),
        ForAll("x")(φ(F($))) -> ForAll("x")(ElementOf(Singleton($), $.^)),
        F($) -> Singleton($))(
        getVariableDefinitions(Seq(φ -> 1), Seq(F -> 0)))
    }

    "correctly apply substitutions to a bound predicate" in {
      val F = TermVariablePlaceholder("F", 1)
      OldSubstitutionApplier.applySubstitutions(
        ForAll("x")(φ(F(a))),
        Substitutions(
          Seq(Equals(b, $.^)),
          Seq(EmptySet, $)))(
          ContextWithExternalDepth(1)
      ) must beSuccessfulTry(ForAll("x")(Equals(b, $.^)))
    }

    "match a variable to a parameter at different internal depths" in {
      testSuccessfulMatch(
        1,
        Substitutions(Nil, Seq($, c)),
        Conjunction(Equals(a, b), ForAll("x")(ElementOf(a, $))) -> Conjunction(Equals($, c), ForAll("x")(ElementOf($.^, $))))(
        getVariableDefinitions(Nil, Seq(a -> 0, b -> 0)))
    }

    "allow a bound variable to match itself as an argument if it is within the internal binding of a predicate" in {
      val A = TermVariablePlaceholder("A", 0)
      val B = TermVariablePlaceholder("B", 1)
      val C = TermVariablePlaceholder("C", 2)
      val D = TermVariablePlaceholder("D", 3)
      val X = TermVariablePlaceholder("X", 4)
      val n = TermVariablePlaceholder("n", 5)
      val F = TermVariablePlaceholder("F", 5)

      testSuccessfulMatch(
        1,
        Substitutions(
          Seq(ExistsIn("y", Product(C, D))(
            Conjunction(
              ElementOf(Pair(Pair($.^^(0), $.^^(1)), $), X),
              Equals(n, F($.^^(0), $.^^(1), First($), Second($)))))),
          Seq(A, B)),
        (Equivalence(
          ExistsIn("x", Product(A, B))(
            φ(First($), Second($))),
          ExistsIn("a", A)(
            ExistsIn("b", B)(
                φ($.^, $)))) ->
          Equivalence(
            ExistsIn("x", Product(A, B))(
              ExistsIn("y", Product(C, D))(
                Conjunction(
                  ElementOf(Pair(Pair(First($.^), Second($.^)), $), X),
                  Equals(n, F(First($.^), Second($.^), First($), Second($)))))),
            ExistsIn("a", A)(
              ExistsIn("b", B)(
                ExistsIn("y", Product(C, D))(
                  Conjunction(
                    ElementOf(Pair(Pair($.^^, $.^), $), X),
                    Equals(n, F($.^^, $.^, First($), Second($))))))))))(
        getVariableDefinitions(Seq(φ -> 2), Seq(A -> 0, B -> 0)))
    }
  }
}
