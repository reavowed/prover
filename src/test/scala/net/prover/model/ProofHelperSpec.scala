package net.prover.model

import net.prover.model.Inference.RearrangementType
import net.prover.model.entries.{Axiom, Theorem}
import net.prover.model.expressions.{FunctionParameter, Statement, Term}
import net.prover.model.proof._

class ProofHelperSpec extends ProverSpec {



  "extracting a statement" should {
    val specification = Axiom("Specification", Seq(ForAll("x")(φ(FunctionParameter(0, 0)))), φ(a))
    val modusPonens = Axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
    val zeroIsANaturalNumber = Axiom("0 Is a Natural Number", Nil, ElementOf(Zero, Naturals))
    val successorOfNaturalIsNatural = Axiom("A Successor of a Natural Number Is a Natural Number", Seq(ElementOf(a, Naturals)), ElementOf(Successor(a), Naturals))
    val axioms = Seq(specification, modusPonens, zeroIsANaturalNumber, successorOfNaturalIsNatural)
    def extract(targetStatement: Statement, premises: Seq[Statement]): Option[Step] = {
      val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)
      val stepContext = StepContext.empty(Nil)
      val premiseContext = PremiseContext.justWithPremises(premises, entryContextWithAxioms)
      ProofHelper.extract(
        targetStatement,
        entryContextWithAxioms,
        stepContext,
        premiseContext
      ).map(_.recalculateReferences(stepContext, premiseContext))
    }

    "find a statement via specification" in {
      extract(
        Equals(a, b),
        Seq(ForAll("x")(Equals(FunctionParameter(0, 0), b)))
      ) must beSome(Step.Assertion(
            Equals(a, b),
            specification.summary,
            Seq(Premise.Given(ForAll("x")(Equals(FunctionParameter(0, 0), b)), PremiseReference(0))),
            Substitutions(terms = Map(a -> a), predicates = Map((φ, 1) -> Equals(FunctionParameter(0, 0), b)))))
    }
    "find a statement via specification and modus ponens" in {
      extract(
        Equals(c, b),
        Seq(
          ForAll("x")(Implication(ElementOf(FunctionParameter(0, 0), a), Equals(FunctionParameter(0, 0), b))),
          ElementOf(c, a))
      ) must beSome(Step.Elided(
        Seq(
          Step.Assertion(
            Implication(ElementOf(c, a), Equals(c, b)),
            specification.summary,
            Seq(Premise.Given(ForAll("x")(Implication(ElementOf(FunctionParameter(0, 0), a), Equals(FunctionParameter(0, 0), b))), PremiseReference(0))),
            Substitutions(terms = Map(a -> c), predicates = Map((φ, 1) -> Implication(ElementOf(FunctionParameter(0, 0), a), Equals(FunctionParameter(0, 0), b))))),
          Step.Assertion(
            Equals(c, b),
            modusPonens.summary,
            Seq(
              Premise.Given(Implication(ElementOf(c, a), Equals(c, b)), StepReference(Seq(0))),
              Premise.Given(ElementOf(c, a), PremiseReference(1))),
            Substitutions(statements = Map(φ -> ElementOf(c, a), ψ -> Equals(c, b))))),
        None,
        Some("Simplified")))
    }
    "find a statement via double nested specification and modus ponens" in {
      extract(
        Equals(a, b),
        Seq(
          ForAll("x")(Implication(ElementOf(FunctionParameter(0, 0), A),
            ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B),
              Equals(FunctionParameter(0, 1), FunctionParameter(0, 0)))))),
          ElementOf(a, A),
          ElementOf(b, B))
      ) must beSome(Step.Elided(
        Seq(
          Step.Assertion(
            Implication(ElementOf(a, A), ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B), Equals(a, FunctionParameter(0, 0))))),
            specification.summary,
            Seq(Premise.Given(
              ForAll("x")(Implication(ElementOf(FunctionParameter(0, 0), A),
                ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B),
                  Equals(FunctionParameter(0, 1), FunctionParameter(0, 0)))))),
              PremiseReference(0))),
            Substitutions(
              terms = Map(a -> a),
              predicates = Map((φ, 1) ->
                Implication(ElementOf(FunctionParameter(0, 0), A),
                  ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B),
                    Equals(FunctionParameter(0, 1), FunctionParameter(0, 0)))))))),
          Step.Assertion(
            ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B), Equals(a, FunctionParameter(0, 0)))),
            modusPonens.summary,
            Seq(
              Premise.Given(Implication(ElementOf(a, A), ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B), Equals(a, FunctionParameter(0, 0))))), StepReference(Seq(0))),
              Premise.Given(ElementOf(a, A), PremiseReference(1))),
            Substitutions(statements = Map(φ -> ElementOf(a, A), ψ -> ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B), Equals(a, FunctionParameter(0, 0))))))),
          Step.Assertion(
            Implication(ElementOf(b, B), Equals(a, b)),
            specification.summary,
            Seq(Premise.Given(
              ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B), Equals(a, FunctionParameter(0, 0)))),
              StepReference(Seq(1)))),
            Substitutions(
              terms = Map(a -> b),
              predicates = Map((φ, 1) -> Implication(ElementOf(FunctionParameter(0, 0), B), Equals(a, FunctionParameter(0, 0)))))),
          Step.Assertion(
            Equals(a, b),
            modusPonens.summary,
            Seq(
              Premise.Given(Implication(ElementOf(b, B), Equals(a, b)), StepReference(Seq(2))),
              Premise.Given(ElementOf(b, B), PremiseReference(2))),
            Substitutions(statements = Map(φ -> ElementOf(b, B), ψ -> Equals(a, b))))),
        None,
        Some("Simplified")))
    }

    "find a statement via modus ponens using a known fact" in {
      extract(
        φ(Zero),
        Seq(ForAll("n")(Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))))
      ) must beSome(Step.Elided(
        Seq(
          Step.Assertion(
            Implication(ElementOf(Zero, Naturals), φ(Zero)),
            specification.summary,
            Seq(Premise.Given(ForAll("n")(Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))), PremiseReference(0))),
            Substitutions(
              terms = Map(a -> Zero),
              predicates = Map((φ, 1) -> Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))))),
          Step.Assertion(
            ElementOf(Zero, Naturals),
            zeroIsANaturalNumber.summary,
            Nil,
            Substitutions.empty),
          Step.Assertion(
            φ(Zero),
            modusPonens.summary,
            Seq(
              Premise.Given(Implication(ElementOf(Zero, Naturals), φ(Zero)), StepReference(Seq(0))),
              Premise.Given(ElementOf(Zero, Naturals), StepReference(Seq(1)))),
            Substitutions(statements = Map(φ -> ElementOf(Zero, Naturals), ψ -> φ(Zero))))),
        None,
        Some("Simplified")))
    }

    "find a statement via modus ponens using a premise simplification" in {
      extract(
        φ(Successor(a)),
        Seq(
          ForAll("n")(Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))),
          ElementOf(a, Naturals))
      ) must beSome(Step.Elided(
        Seq(
          Step.Assertion(
            Implication(ElementOf(Successor(a), Naturals), φ(Successor(a))),
            specification.summary,
            Seq(Premise.Given(ForAll("n")(Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))), PremiseReference(0))),
            Substitutions(terms = Map(a -> Successor(a)), predicates = Map((φ, 1) -> Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))))),
          Step.Assertion(
            ElementOf(Successor(a), Naturals),
            successorOfNaturalIsNatural.summary,
            Seq(Premise.Given(ElementOf(a, Naturals), PremiseReference(1))),
            Substitutions(terms = Map(a -> a))),
          Step.Assertion(
            φ(Successor(a)),
            modusPonens.summary,
            Seq(
              Premise.Given(Implication(ElementOf(Successor(a), Naturals), φ(Successor(a))), StepReference(Seq(0))),
              Premise.Given(ElementOf(Successor(a), Naturals), StepReference(Seq(1)))),
            Substitutions(statements = Map(φ -> ElementOf(Successor(a), Naturals), ψ -> φ(Successor(a)))))),
        None,
        Some("Simplified")))
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
  }

  "rewriting a statement" should {
    "rewrite with simplification and expansion" in {
      val firstElement = Axiom("First Element", Nil, Equals(First(Pair(a, b)), a))
      val reverseEquality = Axiom("Reverse equality", Seq(Equals(a, b)), Equals(b, a))
      val equalityIsTransitive = Axiom("Equality Is Transitive", Seq(Equals(a, b), Equals(b, c)), Equals(a, c))
      val substitutionOfEquals = Axiom("Substitution of Equals", Seq(Equals(a, b), φ(a)), φ(b))
      val substitutionOfEqualsIntoFunction = Axiom("Substitution of Equals Into Function", Seq(Equals(a, b)), Equals(F(a), F(b)))
      val axioms = Seq(firstElement, reverseEquality, equalityIsTransitive, substitutionOfEquals, substitutionOfEqualsIntoFunction)
      val entryContextWithAxioms = entryContext.copy(availableEntries = entryContext.availableEntries ++ axioms)

      val premise = Equals(Pair(First(First(Pair(Pair(a, b), c))), b), Pair(c, d))
      val target = Equals(Pair(a, b), Pair(First(Pair(c, d)), d))
      val steps = ProofHelper.rewrite(
        target,
        entryContextWithAxioms,
        PremiseContext.justWithPremises(Seq(premise), entryContextWithAxioms),
        StepContext.empty(Nil))
      steps must beSome

      val theorem = Theorem(
        "Rewrite",
        Seq(premise),
        target,
        steps.get,
        RearrangementType.NotRearrangement
      ).recalculateReferences(entryContextWithAxioms)
      val serializedTheorem = theorem.serializedLines.mkString("\n").stripPrefix("theorem ")
      println(theorem.serializedLines.mapWithIndex((s, i) => s"${"%02d".format(i + 1)} $s").mkString("\n"))
      val parsedTheorem = Theorem.parser(entryContextWithAxioms).parseFromString(serializedTheorem, "Theorem")
      parsedTheorem mustEqual theorem
    }
  }
}
