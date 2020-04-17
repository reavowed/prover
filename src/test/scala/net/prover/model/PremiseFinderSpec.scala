package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.entries.ExpressionDefinition.{ComponentArgument, ComponentType}
import net.prover.model.entries.{Axiom, PropertyDefinitionOnType, StandalonePropertyDefinition, StatementDefinition, TermDefinition, TypeDefinition}
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
      PremiseFinder.findPremiseStepsForStatement(target)(entryContextAndStepContextToStepProvingContext(entryContext, stepContext)).map(_.steps)
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

    "find a premise by a conclusion simplification from extracting a term definition" in {
      val Negated = TermDefinition(
        "negatedZ",
        Nil,
        Seq(ComponentType.TermComponent("a", Nil)),
        None,
        Some("negated integer"),
        Format.Explicit("-a", Seq("a"), false, true),
        Seq(ElementOf(a, Naturals)),
        Conjunction(ElementOf($, Naturals), Equals(add($, a), Zero)),
        None,
        Nil,
        Nil)
      val entryContextWithDefinition = defaultEntryContext.addEntry(Negated)

      checkFindPremise(
        ElementOf(Negated(a), Naturals),
        ElementOf(a, Naturals))(
        entryContextWithDefinition)
    }

    "find a premise by a conclusion simplification from extracting a term definition with multiple premises" in {
      val PairIsInteger = Axiom("Pair Is Integer", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), ElementOf(Pair(a, b), Integers))
      val entryContextWithDefinitions = defaultEntryContext.addEntry(IntegersDefinition).addEntry(PairIsInteger)

      checkFindPremise(
        ElementOf(Pair(a, b), Integers),
        ElementOf(a, Naturals), ElementOf(b, Naturals))(
        entryContextWithDefinitions)
    }

    "chain conclusion relation simplification definitions using premise substitutions" in {
      checkFindPremise(
        ElementOf(Pair(a, b), Product(A, B)),
        ElementOf(a, A), ElementOf(b, C), Subset(C, B))
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

    "simplify a conclusion by converting a complex defined term into a simpler one" in {
      val PositiveNaturalsDefinition = TermDefinition("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val DefinitionOfPositiveNatural = Axiom("Definition of Positive Natural", Nil, ForAll("n")(Equivalence(ElementOf($, PositiveNaturals), Conjunction(ElementOf($, Naturals), lessThan(Zero, $)))))

      val entryContextWithDefinitions = defaultEntryContext
        .addEntry(PositiveNaturalsDefinition)
        .addEntry(DefinitionOfPositiveNatural)

      checkFindPremise(
        ElementOf(add(a, b), Naturals),
        ElementOf(a, Naturals), ElementOf(b, PositiveNaturals))(
        entryContextWithDefinitions)
    }

    "rewrite a premise using a fact" in {
      val PositiveNaturalsDefinition = TermDefinition("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val PositiveNaturalsAreASubsetOfTheNaturals = Axiom("Positive Naturals Are a Subset of the Naturals", Nil, Subset(PositiveNaturals, Naturals))

      val entryContextWithDefinitions = defaultEntryContext
        .addEntry(PositiveNaturalsDefinition)
        .addEntry(PositiveNaturalsAreASubsetOfTheNaturals)

      checkFindPremise(
        ElementOf(Pair(a, b), Product(Naturals, Naturals)),
        ElementOf(a, Naturals), ElementOf(b, PositiveNaturals))(
        entryContextWithDefinitions)
    }

    "find a premise using an or condition" in {
      checkFindPremise(Disjunction(φ, ψ), φ)
    }

    "find a premise using chained simplifications requiring other premises" in {
      val SetDifference = TermDefinition(
        "diff",
        Nil,
        Seq(ComponentType.TermComponent("A", Nil), ComponentType.TermComponent("B", Nil)),
        None,
        Some("Set Difference"),
        Format.Explicit("%1/%2", "A/B", false, true),
        Nil,
        ForAll("a")(Equivalence(ElementOf($, $.^), Conjunction(ElementOf($, A), Negation(ElementOf($, B))))),
        None,
        Nil,
        Nil)
      val ToInteger = TermDefinition(
        "toZ",
        Nil,
        Seq(ComponentType.TermComponent("a", Nil)),
        None,
        None,
        Format.Explicit("%1_ℤ", "a_ℤ", false, true),
        Nil,
        BlankDefinition,
        None,
        Nil,
        Nil)
      val EqualityConditionForEmbeddedNaturals = Axiom("Equality Condition for Embedded Naturals", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), Equivalence(Equals(a, b), Equals(ToInteger(a), ToInteger(b))))
      val EmbeddedNaturalIsInteger = Axiom("Embedded Natural Is Integer", Seq(ElementOf(a, Naturals)), ElementOf(ToInteger(a), Integers))
      val OneIsNotZero = Axiom("One Is Not Zero", Nil, Negation(Equals(Zero, One)))

      val entryContextWithDefinitions = defaultEntryContext
        .addEntry(SetDifference)
        .addEntry(ToInteger)
        .addEntry(EqualityConditionForEmbeddedNaturals)
        .addEntry(EmbeddedNaturalIsInteger)
        .addEntry(OneIsNotZero)

      checkFindPremise(
        ElementOf(ToInteger(One), SetDifference(Integers, Singleton(ToInteger(Zero)))))(
        entryContextWithDefinitions)
    }

    "find a premise using a rewrite that needs to substitute a term definition to get a valid relation" in {
      val PositiveNaturalsDefinition = TermDefinition("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val Inject = TermDefinition("⍳", Nil, Seq(ComponentType.TermComponent("a", Nil)), None, None, Format.Explicit("%0(%1)", "⍳(a)", false, false), Nil, BlankDefinition, None, Nil, Nil)
      val DefinitionOfPositiveNatural = Axiom("Definition of Positive Natural", Nil, ForAll("n")(Equivalence(ElementOf($, PositiveNaturals), Conjunction(ElementOf($, Naturals), lessThan(Inject(Zero), $)))))
      val Relation = TypeDefinition("relation", "R", Seq("A"), Format.Explicit("on %1", "on A", true, false), None, BlankDefinition)
      val Irreflexive = PropertyDefinitionOnType("irreflexive", Relation, "R", Seq("A"), None, BlankDefinition, Conjunction)
      val elementsRelatedByIrreflexiveNotEqual = Axiom("Elements Related by an Irreflexive Relation Are Not Equal", Seq(Irreflexive.oldStatementDefinition(A, B), ElementOf(Pair(a, b), A)), Negation(Equals(a, b)))
      val lessThanIsIrreflexive = Axiom("< Is Irreflexive", Nil, Irreflexive.oldStatementDefinition(LessThan, Naturals))
      val injectedNaturalIsNatural = Axiom("Injected Natural Is Natural", Seq(ElementOf(a, Naturals)), ElementOf(Inject(a), Naturals))

      val entryContextWithDefinitions = defaultEntryContext
        .addEntry(PositiveNaturalsDefinition)
        .addEntry(Inject)
        .addEntry(DefinitionOfPositiveNatural)
        .addEntry(Relation)
        .addEntry(Irreflexive)
        .addEntry(elementsRelatedByIrreflexiveNotEqual)
        .addEntry(lessThanIsIrreflexive)
        .addEntry(injectedNaturalIsNatural)

      checkFindPremise(
        Negation(Equals(a, Inject(Zero))),
        ElementOf(a, PositiveNaturals))(
        entryContextWithDefinitions)
    }
  }
}
