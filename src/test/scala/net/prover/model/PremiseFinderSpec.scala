package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries.{Axiom, TermDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{PremiseFinder, Step, StepContext}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class PremiseFinderSpec extends Specification {
  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name

  "premise finder" should {
    def checkFindPremise(target: Statement, premises: Statement*)(implicit entryContext: EntryContext): MatchResult[Any] = {
      findPremise(target, premises: _*)(entryContext) must beSome(beStepsThatMakeValidTheorem(premises, target)(entryContext))
    }

    def findPremise(target: Statement, premises: Statement*)(implicit entryContext: EntryContext): Option[Seq[Step]] = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, premises.map(_.requiredSubstitutions).foldTogether.terms.map(_._1))
      PremiseFinder.findPremiseStepsForStatement(target, Nil)(entryContextAndStepContextToStepProvingContext(entryContext, stepContext))
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

    "chain conclusion relation simplification definitions using premise substitutions" in {
      val ElementOfSubsetIsElementOfSet = Axiom("Element of Subset Is Element of Set", Seq(Subset(A, B), ElementOf(a, A)), ElementOf(a, B))
      val entryContextWithDefinition = defaultEntryContext.addEntry(ElementOfSubsetIsElementOfSet)

      checkFindPremise(
        ElementOf(Pair(a, b), Product(A, B)),
        ElementOf(a, A), ElementOf(b, C), Subset(C, B))(
        entryContextWithDefinition)
    }

    "chain conclusion relation simplification definitions using premise substitutions first" in {
      checkFindPremise(
        ElementOf(First(a), C),
        Subset(A, C), ElementOf(a, Product(A, B)))
    }

    "avoid infinite loop with complexity increases" in {
      val ElementOfSuccessorIsElementOfSet = Axiom("Element of Successor Is Element of Set", Seq(ElementOf(a, Successor(b))), ElementOf(a, b))
      val entryContextWithDefinition = defaultEntryContext.addEntry(ElementOfSuccessorIsElementOfSet)

      findPremise(ElementOf(a, b))(entryContextWithDefinition) must beNone
    }

    "avoid infinite loop by passing from complex relation to simpler one" in {
      val LessThanIsElementRelation = Axiom("Less Than Is Element Relation", Seq(lessThan(a, b)), ElementOf(a, b)) // if naively implemented, the premise finder will treat "a < b" as "(a, b) ∈ <" and recurse
      val entryContextWithDefinition = defaultEntryContext.addEntry(LessThanIsElementRelation)

      findPremise(ElementOf(a, b))(entryContextWithDefinition) must beNone
    }
  }
}
