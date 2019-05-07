package net.prover.model

import net.prover.model.Inference.RearrangementType
import net.prover.model.entries.{Axiom, Theorem}
import net.prover.model.expressions.{FunctionParameter, Statement, Term}
import net.prover.model.proof._

class ProofHelperSpec extends ProverSpec {
  "extracting a statement" should {
    val specification = Axiom("Specification", Seq(ForAll("x")(φ(FunctionParameter(0, 0)))), φ(a))
    val modusPonens = Axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
    val reverseImplicationFromEquivalence = Axiom("Reverse Implication from Equivalence", Seq(Equivalence(φ, ψ)), Implication(ψ, φ))
    val zeroIsANaturalNumber = Axiom("0 Is a Natural Number", Nil, ElementOf(Zero, Naturals))
    val successorOfNaturalIsNatural = Axiom("A Successor of a Natural Number Is a Natural Number", Seq(ElementOf(a, Naturals)), ElementOf(Successor(a), Naturals))
    val axioms = Seq(specification, modusPonens, reverseImplicationFromEquivalence, zeroIsANaturalNumber, successorOfNaturalIsNatural)
    val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)
    val stepContext = StepContext.empty(Nil)

    def extract(targetStatement: Statement, premises: Seq[Statement]): Option[Step] = {
      val premiseContext = PremiseContext.justWithPremises(premises, entryContextWithAxioms)
      ProofHelper.extract(
        targetStatement,
        entryContextWithAxioms,
        stepContext,
        premiseContext
      ).map(_.recalculateReferences(stepContext, premiseContext))
    }

    def testExtraction(targetStatement: Statement, premises: Seq[Statement]) = {
      val step = extract(targetStatement, premises)
      step must beSome
      val theorem = Theorem(
        "Rearrangement",
        premises,
        targetStatement,
        step.toSeq,
        RearrangementType.NotRearrangement
      ).recalculateReferences(entryContextWithAxioms)
      val serializedTheorem = theorem.serializedLines.mkString("\n").stripPrefix("theorem ")
      println(serializedTheorem)
      val parsedTheorem = Theorem.parser(entryContextWithAxioms).parseFromString(serializedTheorem, "Theorem")
      parsedTheorem mustEqual theorem
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
  }

  "rearranging a statement" should {
    "rearrange with associativity and commutativity" in {
      def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))
      val reverseEquality = Axiom("Reverse Equality", Seq(Equals(a, b)), Equals(b, a))
      val equalityIsTransitive = Axiom("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
      val substitutionOfEquals = Axiom("Substitution of Equals", Seq(Equals(a, b)), Equals(F(a), F(b)))
      val additionIsAssociative = Axiom("Addition Is Associative", Nil, Equals(add(a, add(b, c)), add(add(a, b), c)))
      val additionIsCommutative = Axiom("Addition Is Commutative", Nil, Equals(add(a, b), add(b, a)))
      val axioms = Seq(reverseEquality, equalityIsTransitive, substitutionOfEquals, additionIsAssociative, additionIsCommutative)
      val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)
      val stepContext = StepContext.empty(Nil)
      val conclusion = Equals(
        add(add(a, b), add(c, d)),
        add(add(a, c), add(b, d)))
      val step = ProofHelper.rearrange(
        conclusion,
        entryContextWithAxioms,
        PremiseContext.justWithPremises(Nil, entryContextWithAxioms),
        stepContext)
      step must beSome
      val theorem = Theorem(
        "Rearrangement",
        Nil,
        conclusion,
        step.toSeq,
        RearrangementType.NotRearrangement
      ).recalculateReferences(entryContextWithAxioms)
      val serializedTheorem = theorem.serializedLines.mkString("\n").stripPrefix("theorem ")
      println(serializedTheorem)
      val parsedTheorem = Theorem.parser(entryContextWithAxioms).parseFromString(serializedTheorem, "Theorem")
      parsedTheorem mustEqual theorem
    }

    "rearrange using a premise in same order" in {
      def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))
      val reverseEquality = Axiom("Reverse Equality", Seq(Equals(a, b)), Equals(b, a))
      val equalityIsTransitive = Axiom("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
      val substitutionOfEquals = Axiom("Substitution of Equals", Seq(Equals(a, b)), Equals(F(a), F(b)))
      val additionIsAssociative = Axiom("Addition Is Associative", Nil, Equals(add(a, add(b, c)), add(add(a, b), c)))
      val additionIsCommutative = Axiom("Addition Is Commutative", Nil, Equals(add(a, b), add(b, a)))
      val axioms = Seq(reverseEquality, equalityIsTransitive, substitutionOfEquals, additionIsAssociative, additionIsCommutative)
      val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)
      val stepContext = StepContext.empty(Nil)
      val premise = Equals(add(a, b), add(c, d))
      val conclusion = Equals(add(d, c), add(b, a))
      val step = ProofHelper.rearrange(
        conclusion,
        entryContextWithAxioms,
        PremiseContext.justWithPremises(Seq(premise), entryContextWithAxioms),
        stepContext)
      step must beSome
      val theorem = Theorem(
        "Rearrangement",
        Seq(premise),
        conclusion,
        step.toSeq,
        RearrangementType.NotRearrangement
      ).recalculateReferences(entryContextWithAxioms)
      val serializedTheorem = theorem.serializedLines.mkString("\n").stripPrefix("theorem ")
      println(serializedTheorem)
      val parsedTheorem = Theorem.parser(entryContextWithAxioms).parseFromString(serializedTheorem, "Theorem")
      parsedTheorem mustEqual theorem
    }

    "rearrange using a premise in reversed order" in {
      def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))
      val reverseEquality = Axiom("Reverse Equality", Seq(Equals(a, b)), Equals(b, a))
      val equalityIsTransitive = Axiom("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
      val substitutionOfEquals = Axiom("Substitution of Equals", Seq(Equals(a, b)), Equals(F(a), F(b)))
      val additionIsAssociative = Axiom("Addition Is Associative", Nil, Equals(add(a, add(b, c)), add(add(a, b), c)))
      val additionIsCommutative = Axiom("Addition Is Commutative", Nil, Equals(add(a, b), add(b, a)))
      val axioms = Seq(reverseEquality, equalityIsTransitive, substitutionOfEquals, additionIsAssociative, additionIsCommutative)
      val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)
      val stepContext = StepContext.empty(Nil)
      val premise = Equals(add(a, b), add(c, d))
      val conclusion = Equals(add(b, a), add(d, c))
      val step = ProofHelper.rearrange(
        conclusion,
        entryContextWithAxioms,
        PremiseContext.justWithPremises(Seq(premise), entryContextWithAxioms),
        stepContext)
      step must beSome
      val theorem = Theorem(
        "Rearrangement",
        Seq(premise),
        conclusion,
        step.toSeq,
        RearrangementType.NotRearrangement
      ).recalculateReferences(entryContextWithAxioms)
      val serializedTheorem = theorem.serializedLines.mkString("\n").stripPrefix("theorem ")
      println(serializedTheorem)
      val parsedTheorem = Theorem.parser(entryContextWithAxioms).parseFromString(serializedTheorem, "Theorem")
      parsedTheorem mustEqual theorem
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

      val stepOption = ProofHelper.rewrite(
        target,
        entryContextWithAxioms,
        PremiseContext.justWithPremises(premises, entryContextWithAxioms),
        StepContext.empty(Nil))
      stepOption must beSome

      val theorem = Theorem(
        "Rewrite",
        premises,
        target,
        stepOption.toSeq,
        RearrangementType.NotRearrangement
      ).recalculateReferences(entryContextWithAxioms)
      val serializedTheorem = theorem.serializedLines.mkString("\n").stripPrefix("theorem ")
      println(theorem.serializedLines.mapWithIndex((s, i) => s"${"%02d".format(i + 1)} $s").mkString("\n"))
      val parsedTheorem = Theorem.parser(entryContextWithAxioms).parseFromString(serializedTheorem, "Theorem")
      parsedTheorem mustEqual theorem
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
  }
}
