package net.prover.model

import net.prover.model.Inference.RearrangementType
import net.prover.model.entries.{Axiom, Theorem}
import net.prover.model.expressions.{FunctionParameter, Statement, Term}
import net.prover.model.proof._
import org.specs2.execute.Result
import org.specs2.matcher.MatchResult

class ProofHelperSpec extends ProverSpec {

  def validateStep(step: Option[Step], targetStatement: Statement, premises: Seq[Statement], entryContext: EntryContext): MatchResult[Any] = {
    step must beSome
    val theorem = Theorem(
      "Test Theorem",
      premises,
      targetStatement,
      Seq(Theorem.Proof(step.toSeq)),
      RearrangementType.NotRearrangement
    ).recalculateReferences(entryContext)
    val serializedTheorem = theorem.serializedLines.mkString("\n").stripPrefix("theorem ")
    val parsedTheorem = Theorem.parser(entryContext).parseFromString(serializedTheorem, "Theorem")
    parsedTheorem mustEqual theorem
  }

  "extracting a statement" should {
    val specification = Axiom("Specification", Seq(ForAll("x")(φ(FunctionParameter(0, 0)))), φ(a))
    val modusPonens = Axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
    val reverseImplicationFromEquivalence = Axiom("Reverse Implication from Equivalence", Seq(Equivalence(φ, ψ)), Implication(ψ, φ))
    val combineConjunction = Axiom("Combine Conjunction", Seq(φ, ψ), Conjunction(φ, ψ))
    val zeroIsANaturalNumber = Axiom("0 Is a Natural Number", Nil, ElementOf(Zero, Naturals))
    val successorOfNaturalIsNatural = Axiom("A Successor of a Natural Number Is a Natural Number", Seq(ElementOf(a, Naturals)), ElementOf(Successor(a), Naturals))
    val axioms = Seq(specification, modusPonens, reverseImplicationFromEquivalence, combineConjunction, zeroIsANaturalNumber, successorOfNaturalIsNatural)
    val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)

    def extract(targetStatement: Statement, premises: Seq[Statement], depth: Int = 0): Option[Step] = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, Nil, entryContextWithAxioms).copy(boundVariableLists = (1 to depth).map(i => Seq(i.toString)))
      SubstatementExtractor.extract(targetStatement)
        .map(_.recalculateReferences(stepContext))
    }

    def testExtraction(targetStatement: Statement, premises: Seq[Statement]) = {
      val step = extract(targetStatement, premises)
      validateStep(step, targetStatement, premises, entryContextWithAxioms)
    }

    "find a statement via specification" in {
      testExtraction(
        Equals(a, b),
        Seq(ForAll("x")(Equals(FunctionParameter(0, 0), b))))
    }
    "find a statement via specification and modus ponens" in {
      testExtraction(
        Equals(c, b),
        Seq(
          ForAll("x")(Implication(ElementOf(FunctionParameter(0, 0), a), Equals(FunctionParameter(0, 0), b))),
          ElementOf(c, a)))
    }
    "find a statement via double nested specification and modus ponens" in {
      testExtraction(
        Equals(a, b),
        Seq(
          ForAll("x")(Implication(ElementOf(FunctionParameter(0, 0), A),
            ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B),
              Equals(FunctionParameter(0, 1), FunctionParameter(0, 0)))))),
          ElementOf(a, A),
          ElementOf(b, B)))
    }

    "find a statement via modus ponens using a known fact" in {
      testExtraction(
        φ(Zero),
        Seq(ForAll("n")(Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0))))))
    }

    "find a statement via modus ponens using a premise simplification" in {
      testExtraction(
        φ(Successor(a)),
        Seq(
          ForAll("n")(Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))),
          ElementOf(a, Naturals)))
    }

    "find a statement via modus ponens using a rewrite" in {
      testExtraction(
        ψ(a),
        Seq(
          ForAll("n")(Equivalence(ψ(FunctionParameter(0, 0)), φ(FunctionParameter(0, 0)))),
          φ(a)))
    }

    "find a statement with a bound variable appearing only in a subsidiary premise" in {
      testExtraction(
        φ(b),
        Seq(
          ForAll("x")(Implication(φ(FunctionParameter(0, 0)), φ(b))),
          φ(a)))
    }

    "find a statement with a bound variable appearing only in a subsidiary premise that requires simplification" in {
      testExtraction(
        φ(a, c),
        Seq(
          ForAll("a")(ForAll("b")(ForAll("c")(Implication(
            Conjunction(
              φ(FunctionParameter(0, 2), FunctionParameter(0, 1)),
              φ(FunctionParameter(0, 1), FunctionParameter(0, 0))),
            φ(FunctionParameter(0, 2), FunctionParameter(0, 0)))))),
          φ(a, b),
          φ(b, c)))
    }

    "not extract statements that are substitution matches but not exact matches" in {
      extract(
        ElementOf(a, b),
        Seq(
          φ,
          Implication(φ, ElementOf(a, c)))
      ) must beNone
      extract(
        φ,
        Seq(ElementOf(a, b))
      ) must beNone
    }

    "not find spurious results with external bound variables" in {
      extract(
        Exists("x")(ψ(FunctionParameter(0, 0) /*x*/, FunctionParameter(0, 2) /*n*/, Successor(FunctionParameter(0, 1))  /*m+*/)),
        Seq(
          ForAll("y")(Implication(
            φ(FunctionParameter(0, 2) /*n*/, FunctionParameter(0, 0) /*y*/),
            Exists("x")(ψ(FunctionParameter(0, 0) /*x*/, FunctionParameter(0, 3) /*n*/, FunctionParameter(0, 1) /*y*/)))),
          φ(FunctionParameter(0, 1) /*n*/, Successor(FunctionParameter(0, 1)) /*n+*/)),
        2
      ) must beNone
    }

    "find non-spurious result with external bound variables" in {
      extract(
        Exists("x")(ψ(FunctionParameter(0, 0), FunctionParameter(0, 2), Successor(FunctionParameter(0, 1)))),
        Seq(
          ForAll("y")(Implication(
            φ(FunctionParameter(0, 2), FunctionParameter(0, 0)),
            Exists("x")(ψ(FunctionParameter(0, 0), FunctionParameter(0, 3), FunctionParameter(0, 1))))),
          φ(FunctionParameter(0, 1), Successor(FunctionParameter(0, 0)))),
        2
      ) must beSome
    }
  }

  "rearranging a statement" should {
    def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))
    val reverseEquality = Axiom("Reverse Equality", Seq(Equals(a, b)), Equals(b, a))
    val equalityIsTransitive = Axiom("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
    val substitutionOfEquals = Axiom("Substitution of Equals", Seq(Equals(a, b), φ(a)), φ(b))
    val substitutionOfEqualsIntoFunction = Axiom("Substitution of Equals Into Function", Seq(Equals(a, b)), Equals(F(a), F(b)))
    val additionIsAssociative = Axiom("Addition Is Associative", Nil, Equals(add(a, add(b, c)), add(add(a, b), c)))
    val additionIsCommutative = Axiom("Addition Is Commutative", Nil, Equals(add(a, b), add(b, a)))
    val axioms = Seq(reverseEquality, equalityIsTransitive, substitutionOfEquals, substitutionOfEqualsIntoFunction, additionIsAssociative, additionIsCommutative)
    val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)

    def rearrange(targetStatement: Statement, premises: Seq[Statement]): Option[Step] = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, Nil, entryContextWithAxioms)
      TermRearranger.rearrange(targetStatement)
        .map(_.recalculateReferences(stepContext))
    }

    def testRearranging(targetStatement: Statement, premises: Seq[Statement]) = {
      val step = rearrange(targetStatement, premises)
      validateStep(step, targetStatement, premises, entryContextWithAxioms)
    }

    "rearrange with associativity and commutativity" in {
      val conclusion = Equals(
        add(add(a, b), add(c, d)),
        add(add(a, c), add(b, d)))
      testRearranging(conclusion, Nil)
    }

    "rearrange using a premise in same order" in {
      val premise = Equals(add(a, b), add(c, d))
      val conclusion = Equals(add(d, c), add(b, a))
      testRearranging(conclusion, Seq(premise))
    }

    "rearrange using a premise in reversed order" in {
      val premise = Equals(add(a, b), add(c, d))
      val conclusion = Equals(add(b, a), add(d, c))
      testRearranging(conclusion, Seq(premise))
    }

    "rearrange inside a function" in {
      val conclusion = Equals(F(add(add(a, b), add(c, d)), add(c, d)), F(add(add(a, c), add(b, d)), add(d, c)))
      testRearranging(conclusion, Nil)
    }
  }

  "rewriting a statement" should {

    def testRewrite(premises: Seq[Statement], target: Statement) = {
      val elementOfCartesianProductFromCoordinates = Axiom("Element of Cartesian Product from Coordinates", Seq(ElementOf(a, Product(A, B))), Equals(a, Pair(First(a), Second(a))))
      val firstCoordinateOfElementOfCartesianProduct = Axiom("First Coordinate of Element of Cartesian Product", Seq(ElementOf(a, Product(A, B))), ElementOf(First(a), A))
      val secondCoordinateOfElementOfCartesianProduct = Axiom("Second Coordinate of Element of Cartesian Product", Seq(ElementOf(a, Product(A, B))), ElementOf(Second(a), B))
      val firstElement = Axiom("First Element", Nil, Equals(First(Pair(a, b)), a))
      val reverseEquality = Axiom("Reverse equality", Seq(Equals(a, b)), Equals(b, a))
      val equalityIsTransitive = Axiom("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
      val substitutionOfEquals = Axiom("Substitution of Equals", Seq(Equals(a, b), φ(a)), φ(b))
      val substitutionOfEqualsIntoFunction = Axiom("Substitution of Equals Into Function", Seq(Equals(a, b)), Equals(F(a), F(b)))
      val axioms = Seq(
        elementOfCartesianProductFromCoordinates,
        firstCoordinateOfElementOfCartesianProduct,
        secondCoordinateOfElementOfCartesianProduct,
        firstElement,
        reverseEquality,
        equalityIsTransitive,
        substitutionOfEquals,
        substitutionOfEqualsIntoFunction)
      val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)

      val stepOption = EqualityRewriter.rewrite(
        target,
        StepContext.withPremisesAndTerms(premises, Nil, entryContextWithAxioms))
      validateStep(stepOption, target, premises, entryContextWithAxioms)

      def checkSteps(steps: Seq[Step]): Result = {
        Result.foreach(steps) { step =>
          step.provenStatement must beSome(beNone ^^ ((s: Statement) => Equals(a, a).calculateSubstitutions(s)(SubstitutionContext.outsideProof)))
          checkSteps(step.asOptionalInstanceOf[Step.WithSubsteps].toSeq.flatMap(_.substeps))
        }
      }
      checkSteps(stepOption.toSeq)
    }

    "rewrite with simplification and expansion" in {
      val premise = Equals(Pair(First(First(Pair(Pair(a, b), c))), b), Pair(c, d))
      val target = Equals(Pair(a, b), Pair(First(Pair(c, d)), d))
      testRewrite(Seq(premise), target)
    }

    "rewrite with a premise requiring complicated simplification" in {
      val premise = ElementOf(a, Product(Product(A, B), Product(C, D)))
      val target = Equals(a, Pair(Pair(First(First(a)), Second(First(a))), Pair(First(Second(a)), Second(Second(a)))))
      testRewrite(Seq(premise), target)
    }

    "rewrite inline" in {
      def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))

      val premises = Seq(Equals(a, b), Equals(c, d))
      val target = Equals(add(a, c), add(b, d))
      testRewrite(premises, target)
    }

    "rewrite inline with simplification and known equality" in {
      def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))

      val premises = Seq(Equals(a, b))
      val target = Equals(add(a, First(Pair(c, d))), add(b, c))
      testRewrite(premises, target)
    }

    "rewrite a function application inline" in {
      val premises = Seq(Equals(a, b))
      val target = Equals(F(a), F(b))
      testRewrite(premises, target)
    }
  }
}
