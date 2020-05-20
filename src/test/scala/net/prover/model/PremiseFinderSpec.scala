package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.Qualifier
import net.prover.model.entries.{Axiom, PropertyDefinitionOnType, TermDefinitionEntry, TypeDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{PremiseFinder, PremiseReference, Step, StepContext, StepReference, SubstitutionContext}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class PremiseFinderSpec extends Specification {
  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name

  "premise finder" should {
    def getStepContext(premises: Seq[Statement], depth: Int): StepContext = {
      val emptyContext = StepContext(StepReference(Nil), premises.map(_.requiredSubstitutions).foldTogether.terms.map(_._1), Nil, Nil)
      val contextWithDepth = (0 until depth).foldLeft(emptyContext){ (stepContext, i) => stepContext.addBoundVariable(s"$$$i")}
      premises.zipWithIndex.foldLeft(contextWithDepth) { case (context, (premise, index)) =>
        context.addStatement(premise, PremiseReference(index))
      }
    }

    def checkFindPremise(target: Statement, premises: Seq[Statement], depth: Int = 0)(implicit entryContext: EntryContext): MatchResult[Any] = {
      findPremise(target, premises, depth)(entryContext) must beSome(beStepsThatMakeValidTheorem(premises, target, depth)(entryContext))
    }

    def findPremise(target: Statement, premises: Seq[Statement], depth: Int = 0)(implicit entryContext: EntryContext): Option[Seq[Step]] = {
      PremiseFinder.findDerivationForStatement(target)(entryContextAndStepContextToStepProvingContext(entryContext, getStepContext(premises, depth))).map(_.steps)
    }

    def findPremiseOrTarget(target: Statement, premises: Seq[Statement], depth: Int = 0)(implicit entryContext: EntryContext): (Seq[Step], Seq[Statement]) = {
      implicit val stepContext = getStepContext(premises, depth)
      PremiseFinder.findDerivationsOrTargets(Seq(target))(entryContextAndStepContextToStepProvingContext(entryContext, getStepContext(premises, depth)))
        .mapLeft(_.steps)
        .mapRight(_.map(_.statement))
    }

    "find premise using rewrite" in {
      checkFindPremise(
        Equals(a, b),
        Seq(Equals(b, a)))
    }

    "find premise using relation simplification" in {
      checkFindPremise(
        ElementOf(First(a), b),
        Seq(ElementOf(a, Product(b, c))))
    }

    "find premise using extracted relation simplification" in {
      checkFindPremise(
        Equals(a, b),
        Seq(ElementOf(a, Singleton(b))))
    }

    "find premise using multiple relation simplifications" in {
      checkFindPremise(
        Equals(First(a), b),
        Seq(ElementOf(a, Product(Singleton(b), c))))
    }

    "find premise using relation simplification in structural simplification" in {
      checkFindPremise(
        ElementOf(First(a), b),
        Seq(Conjunction(ElementOf(a, Product(b, c)), φ)))
    }

    "find premise by simplifying target" in {
      checkFindPremise(
        ElementOf(add(First(a), Second(a)), Naturals),
        Seq(ElementOf(a, Product(Naturals, Naturals))))
    }

    "find premise by simplification" in {
      checkFindPremise(
        ElementOf(a, A),
        Seq(ElementOf(Pair(a, b), Product(A, B))))
    }

    "find premise by double simplification" in {
      checkFindPremise(
        ElementOf(a, A),
        Seq(ElementOf(Pair(Pair(a, b), Pair(c, d)), Product(Product(A, B), Product(C, D)))))
    }

    "find a premise by a conclusion simplification from extracting a term definition" in {
      val Negated = TermDefinitionEntry(
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
        Seq(ElementOf(a, Naturals)))(
        entryContextWithDefinition)
    }

    "find a premise by a conclusion simplification from extracting a term definition with multiple premises" in {
      val PairIsInteger = Axiom("Pair Is Integer", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), ElementOf(Pair(a, b), Integers))
      val entryContextWithDefinitions = defaultEntryContext.addEntry(PairIsInteger)

      checkFindPremise(
        ElementOf(Pair(a, b), Integers),
        Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)))(
        entryContextWithDefinitions)
    }

    "chain conclusion relation simplification definitions using premise substitutions" in {
      checkFindPremise(
        ElementOf(Pair(a, b), Product(A, B)),
        Seq(ElementOf(a, A), ElementOf(b, C), Subset(C, B)))
    }

    "chain conclusion relation simplification definitions using premise substitutions first" in {
      checkFindPremise(
        ElementOf(First(a), C),
        Seq(Subset(A, C), ElementOf(a, Product(A, B))))
    }

    "avoid infinite loop with complexity increases" in {
      val ElementOfSuccessorIsElementOfSet = Axiom("Element of Successor Is Element of Set", Seq(ElementOf(a, Successor(b))), ElementOf(a, b))
      val entryContextWithDefinition = defaultEntryContext.addEntry(ElementOfSuccessorIsElementOfSet)

      findPremise(ElementOf(a, b), Nil)(entryContextWithDefinition) must beNone
    }

    "avoid infinite loop by passing from complex relation to simpler one" in {
      val LessThanIsElementRelation = Axiom("Less Than Is Element Relation", Seq(lessThan(a, b)), ElementOf(a, b)) // if naively implemented, the premise finder will treat "a < b" as "(a, b) ∈ <" and recurse
      val entryContextWithDefinition = defaultEntryContext.addEntry(LessThanIsElementRelation)

      findPremise(ElementOf(a, b), Nil)(entryContextWithDefinition) must beNone
    }

    "simplify a conclusion by converting a complex defined term into a simpler one" in {
      val PositiveNaturalsDefinition = TermDefinitionEntry("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val DefinitionOfPositiveNatural = Axiom("Definition of Positive Natural", Nil, ForAll("n")(Equivalence(ElementOf($, PositiveNaturals), Conjunction(ElementOf($, Naturals), lessThan(Zero, $)))))

      val entryContextWithDefinitions = defaultEntryContext
        .addEntry(PositiveNaturalsDefinition)
        .addEntry(DefinitionOfPositiveNatural)

      checkFindPremise(
        ElementOf(add(a, b), Naturals),
        Seq(ElementOf(a, Naturals), ElementOf(b, PositiveNaturals)))(
        entryContextWithDefinitions)
    }

    "rewrite a premise using a fact" in {
      val PositiveNaturalsDefinition = TermDefinitionEntry("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val PositiveNaturalsAreASubsetOfTheNaturals = Axiom("Positive Naturals Are a Subset of the Naturals", Nil, Subset(PositiveNaturals, Naturals))

      val entryContextWithDefinitions = defaultEntryContext
        .addEntry(PositiveNaturalsDefinition)
        .addEntry(PositiveNaturalsAreASubsetOfTheNaturals)

      checkFindPremise(
        ElementOf(Pair(a, b), Product(Naturals, Naturals)),
        Seq(ElementOf(a, Naturals), ElementOf(b, PositiveNaturals)))(
        entryContextWithDefinitions)
    }

    "find a premise using an or condition" in {
      checkFindPremise(Disjunction(φ, ψ), Seq(φ))
    }

    "find a premise using chained simplifications requiring other premises" in {
      val SetDifference = simpleTermDefinition(
        "diff",
        Seq(A, B),
        Format.Explicit("%1/%2", "A/B", 3, false, true),
        Nil,
        ForAll("a")(Equivalence(ElementOf($, $.^), Conjunction(ElementOf($, A), Negation(ElementOf($, B))))))
      val ToInteger = simpleTermDefinition(
        "toZ",
        Seq(a),
        Format.Explicit("%1_ℤ", "a_ℤ", 2, false, true))
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
        ElementOf(ToInteger(One), SetDifference(Integers, Singleton(ToInteger(Zero)))),
        Nil)(
        entryContextWithDefinitions)
    }

    "find a premise using a rewrite that needs to substitute a term definition to get a valid relation" in {
      // a ≠ ⍳(0)
      // ⍳(0) < a
      // a ∈ ℕ^+
      val PositiveNaturalsDefinition = TermDefinitionEntry("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val Inject = TermDefinitionEntry("⍳", Nil, Seq(ComponentType.TermComponent("a", Nil)), None, None, Format.Explicit("%0(%1)", "⍳(a)", 2, false, false), Nil, BlankDefinition, None, Nil, Nil)
      val DefinitionOfPositiveNatural = Axiom("Definition of Positive Natural", Nil, ForAll("n")(Equivalence(ElementOf($, PositiveNaturals), Conjunction(ElementOf($, Naturals), lessThan(Inject(Zero), $)))))
      val Relation = TypeDefinition("relation", "R", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, false))), None, BlankDefinition)
      val Irreflexive = PropertyDefinitionOnType("irreflexive", Relation, None, None, None, None, BlankDefinition, ConjunctionDefinition)
      val elementsRelatedByIrreflexiveNotEqual = Axiom("Elements Related by an Irreflexive Relation Are Not Equal", Seq(Irreflexive.statementDefinition(A, B), ElementOf(Pair(a, b), A)), Negation(Equals(a, b)))
      val lessThanIsIrreflexive = Axiom("< Is Irreflexive", Nil, Irreflexive.statementDefinition(LessThan, Naturals))
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
        Seq(ElementOf(a, PositiveNaturals)))(
        entryContextWithDefinitions)
    }

    "find premise by equality substitution for variable" in {
      checkFindPremise(
        φ(a),
        Seq(φ(b), Equals(a, b)))
      checkFindPremise(
        φ(a),
        Seq(φ(b), Equals(b, a)))
      checkFindPremise(
        φ($),
        Seq(φ($.^), Equals($, $.^)),
        2)
      checkFindPremise(
        φ($),
        Seq(φ($.^), Equals($.^, $)),
        2)
    }

    "find a premise from a fact using the first possible extraction" in {
      // If the latest extraction is used, the premise finder can actually extract that + is a function from the definition of FunctionFrom, which is not the best way of doing it
      val additionProperty = Conjunction(
        ForAllIn("a", Naturals)(Equals(add($, Zero), $)),
        ForAllIn("a", Naturals)(ForAllIn("b", Naturals)(Equals(add($.^, Successor($)), Successor(add($.^, $))))))
      val axiom = Axiom(
        "Function Properties of Natural Addition",
        Nil,
        Conjunction(
          Conjunction(
            Function(Addition),
            FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)),
          additionProperty))

      findPremise(Function(Addition), Nil)(defaultEntryContext.addEntry(axiom)) must beSome(Seq(
        elided(axiom, Seq(
          assertion(axiom, Nil, Nil),
          assertion(extractLeftConjunct, Seq(Conjunction(Function(Addition), FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)), additionProperty), Nil),
          assertion(extractLeftConjunct, Seq(Function(Addition), FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)), Nil))))(SubstitutionContext.outsideProof))
    }

    "replace terms in a target using a fact" in {
      val axiom = Axiom(
        "Function Properties of Natural Addition",
        Nil,
        Conjunction(
          Function(Addition),
          FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)))

      findPremiseOrTarget(
        ForAllIn("x", Domain(Addition))(ForAllIn("y", Domain(Addition))(φ($.^, $))),
        Nil)(
        defaultEntryContext.addEntry(axiom)
      ) mustEqual (
        Seq(
          elided(axiom, Seq(
            elided(axiom, Seq(
              assertion(axiom, Nil, Nil),
              assertion(extractRightConjunct, Seq(Function(Addition), FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)), Nil))),
            elided(FunctionFrom.statementDefinition.deconstructionInference.get, Seq(
              assertion(FunctionFrom.statementDefinition.deconstructionInference.get, Nil, Seq(Addition, Product(Naturals, Naturals), Naturals)),
              assertion(extractRightConjunct, Seq(Function(Addition), Conjunction(Equals(Domain(Addition), Product(Naturals, Naturals)), Subset(Range(Addition), Naturals))), Nil),
              assertion(extractLeftConjunct, Seq(Equals(Domain(Addition), Product(Naturals, Naturals)), Subset(Range(Addition), Naturals)), Nil),
              assertion(reverseEquality, Nil, Seq(Domain(Addition), Product(Naturals, Naturals))))))),
          assertion(substitutionOfEquals, Seq(ForAllIn("x", $.^)(ForAllIn("y", $.^^)(φ($.^, $)))), Seq(Product(Naturals, Naturals), Domain(Addition))))(SubstitutionContext.outsideProof),
        Seq(ForAllIn("x", Product(Naturals, Naturals))(ForAllIn("y", Product(Naturals, Naturals))(φ($.^, $)))))
    }

    "find a premise using a simplification that has a type statement premise" in {
      // There's actually a LOT of complicated stuff going on here - the expected progression is:
      // f(a) ∈ B
      // f(a) ∈ range(f)    via  range(f) ⊆ B     (Definition of From)
      // a ∈ domain(f)      via  f is a function  (Function Application Is Element of Range)
      // a ∈ A              via  domain(f) = A    (Definition of From)

      checkFindPremise(
        ElementOf(Apply(f, a), B),
        Seq(Conjunction(Function(f), FunctionFrom(f, A, B)), ElementOf(a, A)))
    }

    "find a premise using a type statement simplification and multiple renames" in {
      // Now we expect:
      // ⍳(a) ∈ baseSet(+_ℤ)  (target)
      // ⍳(a) ∈ ℤ             (replace property with value)
      // ⍳(a) ∈ range(⍳)       (rewrite via range(⍳) ⊆ ℤ)
      // a ∈ domain(⍳)        (simplification via Function Application Is Element of Range)
      // a ∈ ℕ               (replace property with value)

      val IotaDefinition = simpleTermDefinition("⍳", Nil, Format.default(0), Nil, Conjunction(Function($), FunctionFrom($, Naturals, Integers)))
      val Iota = IotaDefinition()

      checkFindPremise(
        ElementOf(Apply(Iota, a), BaseSet(IntegerAddition)),
        Seq(ElementOf(a, Naturals)))(
        defaultEntryContext.addEntry(IotaDefinition))
    }

    "find a premise using double simplification of a function application" in {
      val IotaDefinition = simpleTermDefinition("⍳", Nil, Format.default(0), Nil, Conjunction(Function($), FunctionFrom($, Naturals, Integers)))
      val Iota = IotaDefinition()
      val DefinitionOfPositiveNatural = Axiom("Embedding Is Unique", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), Equivalence(Equals(Apply(Iota, a), Apply(Iota, b)), Equals(a, b)))

      checkFindPremise(
        Negation(Equals(Apply(Iota, Zero), Apply(Iota, a))),
        Seq(Negation(Equals(a, Zero)), ElementOf(a, Naturals)))(
        defaultEntryContext.addEntries(Seq(IotaDefinition, DefinitionOfPositiveNatural)))
    }
  }
}
