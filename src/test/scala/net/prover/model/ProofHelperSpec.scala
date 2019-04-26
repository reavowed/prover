package net.prover.model

import net.prover.model.entries.Axiom
import net.prover.model.expressions.{FunctionParameter, Statement}
import net.prover.model.proof._

class ProofHelperSpec extends ProverSpec {

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


  "extracting a statement" should {
    "find a statement via specification" in {
      extract(
        Equals(a, b),
        Seq(ForAll("x")(Equals(FunctionParameter(0, 0), b)))
      ) must beSome(Step.Elided(
        Seq(
          Step.Assertion(
            Equals(a, b),
            specification.summary,
            Seq(Premise.Given(ForAll("x")(Equals(FunctionParameter(0, 0), b)), PremiseReference(0))),
            Substitutions(terms = Map(a -> a), predicates = Map((φ, 1) -> Equals(FunctionParameter(0, 0), b))))),
        None,
        Some("Simplified")))
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
}
