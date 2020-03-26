package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries.{Axiom, TermDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{PremiseFinder, StepContext}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class PremiseFinderSpec extends Specification {
    "premise finder" should {

    def checkFindPremise(target: Statement, premises: Statement*)(implicit entryContext: EntryContext): MatchResult[Any] = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, premises.map(_.requiredSubstitutions).foldTogether.terms.map(_._1))
      PremiseFinder.findPremiseStepsForStatement(target)(entryContextAndStepContextToStepProvingContext(entryContext, stepContext)) must beSome(beStepsThatMakeValidTheorem(premises, target)(entryContext))
    }

    "find premise using rewrite" in {
      checkFindPremise(
        Equals(a, b),
        Equals(b, a))
    }

    "find premise using relation simplification" in {
      checkFindPremise(
        ElementOf(First(a), b),
        ElementOf(a, Product(b, c)))
    }

    "find premise using extracted relation simplification" in {
      checkFindPremise(
        Equals(a, b),
        ElementOf(a, Singleton(b)))
    }

    "find premise using multiple relation simplifications" in {
      checkFindPremise(
        Equals(First(a), b),
        ElementOf(a, Product(Singleton(b), c)))
    }

    "find premise using relation simplification in structural simplification" in {
      checkFindPremise(
        ElementOf(First(a), b),
        Conjunction(ElementOf(a, Product(b, c)), φ))
    }

    "find premise by simplifying target" in {
      checkFindPremise(
        ElementOf(add(First(a), Second(a)), Naturals),
        ElementOf(a, Product(Naturals, Naturals)))
    }

    "find premise by simplification" in {
      checkFindPremise(
        ElementOf(a, A),
        ElementOf(Pair(a, b), Product(A, B)))
    }

    "find premise by double simplification" in {
      checkFindPremise(
        ElementOf(a, A),
        ElementOf(Pair(Pair(a, b), Pair(c, d)), Product(Product(A, B), Product(C, D))))
    }

    "find a premise by a left-hand relation simplifcation from extracting a term definition" in {
      val Negated = TermDefinition(
        "negatedZ",
        Nil,
        Seq(ComponentType.TermComponent("a", Nil)),
        Some("negated integer"),
        Format.Explicit("-a", Seq("a"), false, true),
        Seq(ElementOf(a, Naturals)),
        Conjunction(ElementOf($, Naturals), Equals(add($, a), Zero)),
        None,
        Nil)
      val entryContextWithDefinition = defaultEntryContext.addEntry(Negated)

      checkFindPremise(
        ElementOf(Negated(a), Naturals),
        ElementOf(a, Naturals))(
        entryContextWithDefinition)
    }

      "find a premise by a left-hand relation simplification from extracting a term definition" in {
        val IntegerDefinition = TermDefinition(
          "ℤ",
          Nil,
          Nil,
          None,
          Format.default("ℤ", Nil),
          Nil,
          BlankDefinition,
          None,
          Nil)
        val Integers = IntegerDefinition()
        val PairIsInteger = Axiom("Pair Is Integer", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), ElementOf(Pair(a, b), Integers))
        val entryContextWithDefinitions = defaultEntryContext.addEntry(IntegerDefinition).addEntry(PairIsInteger)

        checkFindPremise(
          ElementOf(Pair(a, b), Integers),
          ElementOf(a, Naturals), ElementOf(b, Naturals))(
          entryContextWithDefinitions)
      }
  }
}
