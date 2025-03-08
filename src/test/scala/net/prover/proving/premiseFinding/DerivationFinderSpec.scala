package net.prover.proving.premiseFinding

import net.prover.StepBuilderHelper
import net.prover.model.TestDefinitions._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.Qualifier
import net.prover.model.entries.{ParentTypeConditions, PropertyDefinitionOnType, TermDefinitionEntry, TypeDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.model.{AvailableEntries, Format, TermVariablePlaceholder, TestDefinitions, VariableDefinitions}
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class DerivationFinderSpec extends Specification with StepBuilderHelper {
  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name
  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Seq(a -> 0, b -> 0, c -> 0, d -> 0))

  "premise finder" should {

    def checkFindPremiseSteps(target: Statement, premises: Seq[Statement], steps: SubstitutionContext => Seq[Step], depth: Int = 0)(implicit availableEntries: AvailableEntries, variableDefinitions: VariableDefinitions): MatchResult[Any] = {
      findPremise(target, premises, depth)(availableEntries) must beSome(
        beEmptyOrStepsThatMakeValidAndCompleteTheorem(premises, target, depth) and beEqualTo(steps(SubstitutionContext.withDepth(depth)))
      )
    }

    def checkFindPremise(target: Statement, premises: Seq[Statement], depth: Int = 0)(implicit availableEntries: AvailableEntries, variableDefinitions: VariableDefinitions): MatchResult[Any] = {
      findPremise(target, premises, depth)(availableEntries) must beSome(beStepsThatMakeValidAndCompleteTheorem(premises, target, depth))
    }

    def findPremise(target: Statement, premises: Seq[Statement], depth: Int = 0)(implicit availableEntries: AvailableEntries): Option[Seq[Step]] = {
      implicit val stepContext = createBaseStepContext(premises, depth)
      DerivationFinder.findDerivationForStatement(target).map(_.toProofSteps)
    }

    "find a simplified premise without a derivation" in {
      checkFindPremiseSteps(
        φ,
        Seq(Conjunction(φ, ψ)),
        Nil)
    }

    "find an existing premise with different bound variable names" in {
      checkFindPremiseSteps(
        Exists("x")(ForAll("y")(Equals($, $.^))),
        Seq(Exists("a")(ForAll("b")(Equals($, $.^)))),
        Nil)
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
      checkFindPremiseSteps(
        ElementOf(First(a), b),
        Seq(Conjunction(ElementOf(a, Product(b, c)), φ)),
        Seq(assertion(firstCoordinateOfElementOfCartesianProduct, Nil, Seq(a, b, c))))
    }

    "find premise by simplifying target" in {
      checkFindPremise(
        ElementOf(add(First(a), Second(a)), Naturals),
        Seq(ElementOf(a, Product(Naturals, Naturals))))
    }

    "find premise by simplification" in {
      checkFindPremise(
        ElementOf(a, c),
        Seq(ElementOf(Pair(a, b), Product(c, d))))
    }

    "find premise by double simplification" in {
      val A = TermVariablePlaceholder("A", 4)
      val B = TermVariablePlaceholder("B", 5)
      val C = TermVariablePlaceholder("C", 6)
      val D = TermVariablePlaceholder("D", 7)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, c -> 0, d -> 0, A -> 0, B -> 0, C -> 0, D -> 0))

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
        Format.Explicit("-%1", "-a", 2, false, true),
        Seq(ElementOf(a, Naturals)),
        Conjunction(ElementOf($, Naturals), Equals(add($, a), Zero)),
        None,
        Nil,
        Nil)
      implicit val availableEntries = defaultAvailableEntriesPlus(Negated)

      checkFindPremise(
        ElementOf(Negated(a), Naturals),
        Seq(ElementOf(a, Naturals)))
    }

    "find a premise by a conclusion simplification from extracting a term definition with multiple premises" in {
      val PairIsInteger = createInference("Pair Is Integer", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), ElementOf(Pair(a, b), Integers))
      implicit val availableEntries = defaultAvailableEntriesPlus(PairIsInteger)

      checkFindPremise(
        ElementOf(Pair(a, b), Integers),
        Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)))
    }

    "chain conclusion relation simplification definitions using premise substitutions" in {
      val A = TermVariablePlaceholder("A", 2)
      val B = TermVariablePlaceholder("B", 3)
      val C = TermVariablePlaceholder("C", 4)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, A -> 0, B -> 0, C -> 0))
      checkFindPremise(
        ElementOf(Pair(a, b), Product(A, B)),
        Seq(ElementOf(a, A), ElementOf(b, C), Subset(C, B)))
    }

    "chain conclusion relation simplification definitions using premise substitutions first" in {
      val A = TermVariablePlaceholder("A", 1)
      val B = TermVariablePlaceholder("B", 2)
      val C = TermVariablePlaceholder("C", 3)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, A -> 0, B -> 0, C -> 0))
      checkFindPremise(
        ElementOf(First(a), C),
        Seq(Subset(A, C), ElementOf(a, Product(A, B))))
    }

    "avoid infinite loop with complexity increases" in {
      val ElementOfSuccessorIsElementOfSet = createInference("Element of Successor Is Element of Set", Seq(ElementOf(a, Successor(b))), ElementOf(a, b))
      implicit val availableEntries = defaultAvailableEntriesPlus(ElementOfSuccessorIsElementOfSet)

      findPremise(ElementOf(a, b), Nil) must beNone
    }

    "avoid infinite loop by passing from complex relation to simpler one" in {
      val LessThanIsElementRelation = createInference("Less Than Is Element Relation", Seq(lessThan(a, b)), ElementOf(a, b)) // if naively implemented, the premise finder will treat "a < b" as "(a, b) ∈ <" and recurse
      implicit val availableEntries = defaultAvailableEntriesPlus(LessThanIsElementRelation)

      findPremise(ElementOf(a, b), Nil) must beNone
    }

    "simplify a conclusion by converting a complex defined term into a simpler one" in {
      val PositiveNaturalsDefinition = TermDefinitionEntry("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val DefinitionOfPositiveNatural = createInference("Definition of Positive Natural", Nil, ForAll("n")(Equivalence(ElementOf($, PositiveNaturals), Conjunction(ElementOf($, Naturals), lessThan(Zero, $)))))

      implicit val availableEntries = defaultAvailableEntriesPlus(PositiveNaturalsDefinition, DefinitionOfPositiveNatural)

      checkFindPremise(
        ElementOf(add(a, b), Naturals),
        Seq(ElementOf(a, Naturals), ElementOf(b, PositiveNaturals)))
    }

    "rewrite a premise using a fact" in {
      val PositiveNaturalsDefinition = TermDefinitionEntry("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val PositiveNaturalsAreASubsetOfTheNaturals = createInference("Positive Naturals Are a Subset of the Naturals", Nil, Subset(PositiveNaturals, Naturals))

      implicit val availableEntries = defaultAvailableEntriesPlus(PositiveNaturalsDefinition, PositiveNaturalsAreASubsetOfTheNaturals)

      checkFindPremise(
        ElementOf(Pair(a, b), Product(Naturals, Naturals)),
        Seq(ElementOf(a, Naturals), ElementOf(b, PositiveNaturals)))
    }

    "find a premise using an or condition" in {
      checkFindPremise(Disjunction(φ, ψ), Seq(φ))
    }

    "find a premise using chained simplifications requiring other premises" in {
      val SetDifference = simpleTermDefinition(
        "diff",
        Seq(a, b),
        Format.Explicit("%1/%2", "A/B", 3, false, true),
        Nil,
        ForAll("a")(Equivalence(ElementOf($, $.^), Conjunction(ElementOf($, a), Negation(ElementOf($, b))))))
      val ToInteger = simpleTermDefinition(
        "toZ",
        Seq(a),
        Format.Explicit("%1_ℤ", "a_ℤ", 2, false, true))
      val EqualityConditionForEmbeddedNaturals = createInference("Equality Condition for Embedded Naturals", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), Equivalence(Equals(a, b), Equals(ToInteger(a), ToInteger(b))))
      val EmbeddedNaturalIsInteger = createInference("Embedded Natural Is Integer", Seq(ElementOf(a, Naturals)), ElementOf(ToInteger(a), Integers))
      val OneIsNotZero = createInference("One Is Not Zero", Nil, Negation(Equals(Zero, One)))

      implicit val availableEntries = defaultAvailableEntriesPlus(SetDifference, ToInteger, EqualityConditionForEmbeddedNaturals, EmbeddedNaturalIsInteger, OneIsNotZero)

      checkFindPremise(
        ElementOf(ToInteger(One), SetDifference(Integers, Singleton(ToInteger(Zero)))),
        Nil)
    }

    "find a premise using a rewrite that needs to substitute a term definition to get a valid relation" in {
      // a ≠ ⍳(0)
      // ⍳(0) < a
      // a ∈ ℕ^+
      val PositiveNaturalsDefinition = TermDefinitionEntry("ℕ^+", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, Equals($, Comprehension.bind("a")(Naturals, lessThan(Zero, $))), None, Nil, Nil)
      val PositiveNaturals = PositiveNaturalsDefinition()
      val Inject = TermDefinitionEntry("⍳", Nil, Seq(ComponentType.TermComponent("a", Nil)), None, None, Format.Explicit("%0(%1)", "⍳(a)", 2, false, false), Nil, BlankDefinition, None, Nil, Nil)
      val DefinitionOfPositiveNatural = createInference("Definition of Positive Natural", Nil, ForAll("n")(Equivalence(ElementOf($, PositiveNaturals), Conjunction(ElementOf($, Naturals), lessThan(Inject(Zero), $)))))
      val Relation = TypeDefinition("relation", "R", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, false))), None, BlankDefinition)
      val Irreflexive = PropertyDefinitionOnType("irreflexive", ParentTypeConditions(Relation, None, None, None, ConjunctionDefinition), None, BlankDefinition)
      val elementsRelatedByIrreflexiveNotEqual = createInference("Elements Related by an Irreflexive Relation Are Not Equal", Seq(Irreflexive(a, b), ElementOf(Pair(c, d), a)), Negation(Equals(c, d)))
      val lessThanIsIrreflexive = createInference("< Is Irreflexive", Nil, Irreflexive(LessThan, Naturals))
      val injectedNaturalIsNatural = createInference("Injected Natural Is Natural", Seq(ElementOf(a, Naturals)), ElementOf(Inject(a), Naturals))

      implicit val availableEntries = defaultAvailableEntriesPlus(PositiveNaturalsDefinition, Inject, DefinitionOfPositiveNatural, Relation, Irreflexive, elementsRelatedByIrreflexiveNotEqual, lessThanIsIrreflexive, injectedNaturalIsNatural)

      checkFindPremise(
        Negation(Equals(a, Inject(Zero))),
        Seq(ElementOf(a, PositiveNaturals)))
    }

    "find a premise from a fact using the first possible extraction" in {
      // If the latest extraction is used, the premise finder can actually extract that + is a function from the definition of FunctionFrom, which is not the best way of doing it
      val additionProperty = Conjunction(
        ForAllIn("a", Naturals)(Equals(add($, Zero), $)),
        ForAllIn("a", Naturals)(ForAllIn("b", Naturals)(Equals(add($.^, Successor($)), Successor(add($.^, $))))))
      val axiom = createInference(
        "Function Properties of Natural Addition",
        Nil,
        Conjunction(
          Conjunction(
            Function(Addition),
            FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)),
          additionProperty))

      findPremise(Function(Addition), Nil)(defaultAvailableEntriesPlus(axiom)) must beSome(Seq(
        inferenceExtraction(
          assertion(axiom, Nil, Nil),
          Seq(
            assertion(extractLeftConjunct, Seq(Conjunction(Function(Addition), FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)), additionProperty), Nil),
            assertion(extractLeftConjunct, Seq(Function(Addition), FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)), Nil))))(SubstitutionContext.outsideProof))
    }

    "find a premise using a simplification that has a type statement premise" in {
      // There's actually a LOT of complicated stuff going on here - the expected progression is:
      // f(a) ∈ B
      // f(a) ∈ range(f)    via  range(f) ⊆ B     (Definition of From)
      // a ∈ domain(f)      via  f is a function  (Function Application Is Element of Range)
      // a ∈ A              via  domain(f) = A    (Definition of From)
      val f = TermVariablePlaceholder("f", 0)
      val A = TermVariablePlaceholder("A", 1)
      val B = TermVariablePlaceholder("B", 2)
      val a = TermVariablePlaceholder("a", 3)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(f -> 0, A -> 0, B -> 0, a -> 0))

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

      checkFindPremise(
        ElementOf(Apply(IntegerEmbedding, a), BaseSet(IntegerAddition)),
        Seq(ElementOf(a, Naturals)))
    }

    "find a premise using double simplification of a function application" in {
      val IntegerEmbeddingIsUnique = createInference("Integer Embedding Is Unique", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), Equivalence(Equals(toZ(a), toZ(b)), Equals(a, b)))
      implicit val availableEntries = defaultAvailableEntriesPlus(IntegerEmbeddingIsUnique)
      checkFindPremise(
        Negation(Equals(toZ(Zero), toZ(a))),
        Seq(Negation(Equals(a, Zero)), ElementOf(a, Naturals)))
    }

    "find a premise by equality substitution using a fact" in {
      checkFindPremise(
        ElementOf(add(a, b), Naturals),
        Seq(ElementOf(a, Naturals), ElementOf(b, Domain(IntegerEmbedding))))
    }

    "show that the result of a binary operation application is a member of the named base set" in {
      val ∗ = TermVariablePlaceholder("∗", 0)
      val A = TermVariablePlaceholder("A", 1)
      val a = TermVariablePlaceholder("a", 2)
      val b = TermVariablePlaceholder("b", 3)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(∗ -> 0, A -> 0, a -> 0, b -> 0))

      // a ∈ A / b ∈ A
      // a ∈ baseSet(∗) / b ∈ baseSet(∗)
      // (a, b) ∈ baseSet(∗) × baseSet(∗)
      // (a, b) ∈ domain(∗)                 (via BinaryOperation(∗) -> FunctionFrom(∗, BaseSet(∗) × BaseSet(∗), BaseSet(∗)) -> Range(∗) ⊆ BaseSet(∗))
      // a ∗ b ∈ range(∗)                   (via BinaryOperation(∗) -> Function(∗) -> Function Application Is Element of Range)
      // a ∗ b ∈ baseSet(∗)                 (via BinaryOperation(∗) -> FunctionFrom(∗, BaseSet(∗) × BaseSet(∗), BaseSet(∗)) -> Range(∗) ⊆ BaseSet(∗))
      // a ∗ b ∈ A                          (via BinaryOperationOn(∗, A) -> baseSet(∗) = A)
      checkFindPremiseSteps(
        ElementOf(Apply(∗, Pair(a , b)), A),
        Seq(Conjunction(BinaryOperation(∗), BinaryOperationOn(∗, A)), ElementOf(a, A), ElementOf(b, A)),
        Seq(
          inferenceExtraction(
            assertion(BinaryOperation.deconstructionInference, Nil, Seq(∗)),
            Seq(
              assertion(extractLeftConjunct, Seq(Function(∗), FunctionFrom(∗, Product(BaseSet(∗), BaseSet(∗)), BaseSet(∗))), Nil))), // ∗ is a function
          inferenceExtraction(
            assertion(BinaryOperationOn.deconstructionInference, Nil, Seq(∗, A)),
            Seq(
              assertion(reverseEquality, Nil, Seq(BaseSet(∗), A)))),                                                                  // A = BaseSet(∗)
          assertion(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(A, BaseSet(∗))),                                                // a ∈ baseSet(∗)
          assertion(substitutionOfEquals, Seq(ElementOf(b, $)), Seq(A, BaseSet(∗))),                                                // b ∈ baseSet(∗)
          assertion(orderedPairIsElementOfCartesianProduct, Nil, Seq(a, BaseSet(∗), b, BaseSet(∗))),                                // (a, b) ∈ baseSet(∗) × baseSet(∗)
          inferenceExtraction(
            assertion(BinaryOperation.deconstructionInference, Nil, Seq(∗)),
            Seq(
              assertion(extractRightConjunct, Seq(Function(∗), FunctionFrom(∗, Product(BaseSet(∗), BaseSet(∗)), BaseSet(∗))), Nil))),  // ∗ is from baseSet(∗) × baseSet(∗) to baseSet(∗)
          inferenceExtraction(
            assertion(FunctionFrom.deconstructionInference, Nil, Seq(∗, Product(BaseSet(∗), BaseSet(∗)), BaseSet(∗))),
            Seq(
              assertion(reverseEquality, Nil, Seq(Domain(∗), Product(BaseSet(∗), BaseSet(∗)))))),                                      // baseSet(∗) × baseSet(∗) = domain(∗)
          assertion(substitutionOfEquals, Seq(ElementOf(Pair(a, b), $)), Seq(Product(BaseSet(∗), BaseSet(∗)), Domain(∗))),           // (a, b) ∈ domain(∗)
          assertion(functionApplicationIsElementOfRange, Nil, Seq(∗, Pair(a, b))),                                                   // (a ∗ b) ∈ codomain(∗)
          inferenceExtraction(
            assertion(FunctionFrom.deconstructionInference, Nil, Seq(∗, Product(BaseSet(∗), BaseSet(∗)), BaseSet(∗))),
            Seq(
              assertion(extractRightConjunct, Seq(Function(∗), Conjunction(Equals(Domain(∗), Product(BaseSet(∗), BaseSet(∗))), Subset(Range(∗), BaseSet(∗)))), Nil),
              assertion(extractRightConjunct, Seq(Equals(Domain(∗), Product(BaseSet(∗), BaseSet(∗))), Subset(Range(∗), BaseSet(∗))), Nil))), // codomain(∗) ⊆ baseSet(∗)
          inferenceExtraction(
            assertion(Subset.deconstructionInference.get, Nil, Seq(Range(∗), BaseSet(∗))),
            Seq(
              assertion(specification, Seq(Implication(ElementOf($, Range(∗)), ElementOf($, BaseSet(∗)))), Seq(Apply(∗, Pair(a, b)))),
              assertion(modusPonens, Seq(ElementOf(Apply(∗, Pair(a, b)), Range(∗)), ElementOf(Apply(∗, Pair(a, b)), BaseSet(∗))), Nil))), // (a ∗ b) ∈ baseSet(∗)
          inferenceExtraction(
            assertion(BinaryOperationOn.deconstructionInference, Nil, Seq(∗, A)),
            Seq(
              assertion(extractRightConjunct, Seq(BinaryOperation(∗), Equals(BaseSet(∗), A)), Nil))),                                  // BaseSet(∗) = A
          assertion(substitutionOfEquals, Seq(ElementOf(Apply(∗, Pair(a, b)), $)), Seq(BaseSet(∗), A))))
    }

    "group type relation statements appropriately" in {
      val ∗ = TermVariablePlaceholder("∗", 0)
      val ∘ = TermVariablePlaceholder("∘", 1)
      val a = TermVariablePlaceholder("a", 2)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(∗ -> 0, ∘ -> 0, a -> 0))

      checkFindPremiseSteps(
        ElementOf(a, BaseSet(∗)),
        Seq(Distributive(∗, ∘), ElementOf(a, BaseSet(∘))),
        Seq(
          inferenceExtraction(
            assertion(Distributive.deconstructionInference, Nil, Seq(∗, ∘)),
            Seq(
              assertion(reverseEquality, Nil, Seq(BaseSet(∗), BaseSet(∘))))),
          assertion(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(BaseSet(∘), BaseSet(∗)))))
    }

    "apply multiple type definitions" in {
      val ∘ = TermVariablePlaceholder("∘", 0)
      val f = TermVariablePlaceholder("f", 1)
      val A = TermVariablePlaceholder("A", 2)
      val a = TermVariablePlaceholder("a", 3)
      val b = TermVariablePlaceholder("b", 4)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(∘ -> 0, f -> 0, A -> 0, a -> 0, b -> 0))

      // b ∈ A
      // b ∈ domain(f)
      // f(b) ∈ range(f)
      // f(b) ∈ A
      // f(b) ∈ baseSet(∘)
      // (a, f(b)) ∈ baseSet(∘) × baseSet(∘)
      // (a, f(b)) ∈ domain(∘)
      // a ∘ f(b) ∈ range(∘)
      // a ∘ f(b) ∈ baseSet(∘)
      // a ∘ f(b) ∈ A

      checkFindPremise(
        ElementOf(Apply2(∘, a, Apply(f, b)), A),
        Seq(Conjunction(BinaryOperation(∘), BinaryOperationOn(∘, A)), Conjunction(UnaryOperation(f), UnaryOperationOn(f, A)), ElementOf(a, A), ElementOf(b, A)))
    }

    "find a premise inside a generalized deduction" in {
      val A = TermVariablePlaceholder("A", 0)
      val b = TermVariablePlaceholder("b", 1)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(A -> 0, b -> 0))
      checkFindPremise(
        ForAllIn("a", A)(ElementOf(Pair($, b), Product(A, A))),
        Seq(ElementOf(b, A)))
    }

    "apply multiple type definitions inside generalizations and deductions" in {
      val ∘ = TermVariablePlaceholder("∘", 0)
      val f = TermVariablePlaceholder("f", 1)
      val A = TermVariablePlaceholder("A", 2)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(∘ -> 0, f -> 0, A -> 0))

      checkFindPremise(
        ForAllIn("a", A)(ForAllIn("b", A)(ElementOf(Apply2(∘, $.^, Apply(f, $)), A))),
        Seq(Conjunction(BinaryOperation(∘), BinaryOperationOn(∘, A)), Conjunction(UnaryOperation(f), UnaryOperationOn(f, A))))
    }

    "find a premise by combining an existing premise and a fact" in {
      checkFindPremiseSteps(
        Conjunction(ElementOf(Zero, Naturals), φ),
        Seq(φ),
        Seq(
          assertion(zeroIsANaturalNumber, Nil, Nil),
          assertion(combineConjunction, Seq(ElementOf(Zero, Naturals), φ), Nil)))
    }

    "return a fact rearrangement as a single step" in {
      val R = TermVariablePlaceholder("R", 0)
      val A = TermVariablePlaceholder("A", 1)
      val Relation = TypeDefinition("relation", "R", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, false))), None, BlankDefinition)
      val Antisymmetric = PropertyDefinitionOnType("antisymmetric", ParentTypeConditions(Relation, None, None, None, ConjunctionDefinition), None, BlankDefinition)
      val Transitive = PropertyDefinitionOnType("transitive", ParentTypeConditions(Relation, None, None, None, ConjunctionDefinition), None, BlankDefinition)
      val Complete = PropertyDefinitionOnType("complete", ParentTypeConditions(Relation, None, None, None, ConjunctionDefinition), None, BlankDefinition)
      val TotalOrder = TypeDefinition("totalOrder", "R", Some(Qualifier(Seq("A"), Format.Explicit("on A", Seq("A"), true, false))), None, Conjunction(Conjunction(Conjunction(Relation(R, A), Antisymmetric(R, A)), Transitive(R, A)), Complete(R, A)))
      val StrictOrderDefinition = TermDefinitionEntry("<", Nil, Nil, None, None, Format.default(Nil, Nil), Nil, φ($), None, Nil, Nil)
      val StrictOrder = StrictOrderDefinition()
      val StrictOrderIsATotalOrder = createInference("< is a total order on N", Nil, TotalOrder(StrictOrder, Naturals))

      implicit val availableEntries = defaultAvailableEntries
        .addEntry(Relation)
        .addEntry(Antisymmetric)
        .addEntry(Transitive)
        .addEntry(Complete)
        .addEntry(Complete)
        .addEntry(TotalOrder)
        .addEntry(StrictOrderDefinition)
        .addEntry(StrictOrderIsATotalOrder)

      checkFindPremiseSteps(
        Conjunction(Relation(StrictOrder, Naturals), Transitive(StrictOrder, Naturals)),
        Nil,
        Seq(
          inferenceExtraction(
            assertion(StrictOrderIsATotalOrder, Nil, Nil),
            Seq(
              inferenceExtraction(
                assertion(TotalOrder.deconstructionInference, Nil, Seq(StrictOrder, Naturals)),
                Seq(
                  assertion(extractLeftConjunct, Seq(Conjunction(Conjunction(Relation(StrictOrder, Naturals), Antisymmetric(StrictOrder, Naturals)), Transitive(StrictOrder, Naturals)), Complete(StrictOrder, Naturals)), Nil),
                  assertion(extractLeftConjunct, Seq(Conjunction(Relation(StrictOrder, Naturals), Antisymmetric(StrictOrder, Naturals)), Transitive(StrictOrder, Naturals)), Nil),
                  assertion(extractLeftConjunct, Seq(Relation(StrictOrder, Naturals), Antisymmetric(StrictOrder, Naturals)), Nil))),
              inferenceExtraction(
                assertion(TotalOrder.deconstructionInference, Nil, Seq(StrictOrder, Naturals)),
                Seq(
                  assertion(extractLeftConjunct, Seq(Conjunction(Conjunction(Relation(StrictOrder, Naturals), Antisymmetric(StrictOrder, Naturals)), Transitive(StrictOrder, Naturals)), Complete(StrictOrder, Naturals)), Nil),
                  assertion(extractRightConjunct, Seq(Conjunction(Relation(StrictOrder, Naturals), Antisymmetric(StrictOrder, Naturals)), Transitive(StrictOrder, Naturals)), Nil))),
            assertion(combineConjunction, Seq(Relation(StrictOrder, Naturals), Transitive(StrictOrder, Naturals)), Nil)))))
    }

    "find a premise that has been rewritten with known values" in {
      val f = TermVariablePlaceholder("f", 1)
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1), Seq(f -> 0, a -> 0, b -> 0))
      checkFindPremise(
        ForAll("x")(Implication(ElementOf($, Domain(f)), φ($))),
        Seq(ForAll("x")(Implication(ElementOf($, a), φ($))), Conjunction(Function(f), FunctionFrom(f, a, b))))
    }
  }
}
