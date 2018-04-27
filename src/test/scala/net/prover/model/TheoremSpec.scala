package net.prover.model

import java.nio.file.Paths

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions._
import net.prover.model.entries.{Axiom, StatementDefinition}
import net.prover.model.proof._

class TheoremSpec extends ProverSpec {

  def axiom(
    name: String,
    premises: Seq[PremiseMagnet],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement
  ): Axiom = {
    Axiom(name, name.formatAsKey, "test-chapter", "Test Chapter", "test-book", "Test Book", premises, conclusion, rearrangementType)
  }

  "theorem parser" should {
    implicit def assertionStepFromStatement(assertion: Statement): StepOutline = {
      StepOutline.Assertion(assertion, None, None)
    }

    implicit def assumptionStepFromStatementPair(tuple: (Statement, Statement)): StepOutline = {
      StepOutline.Assumption(tuple._1, Seq(assertionStepFromStatement(tuple._2)), None)
    }

    implicit def assumptionStepFromStatementAndStatements(tuple: (Statement, Seq[Statement])): StepOutline = {
      StepOutline.Assumption(tuple._1, tuple._2.map(assertionStepFromStatement), None)
    }

    implicit class ElidingStatementOps(statement: Statement) {
      def via(elidedStatement: Statement): StepOutline = StepOutline.Assertion(statement, Some(elidedStatement), None)
    }

    def prove(
      premises: Seq[PremiseMagnet],
      proofSteps: Seq[StepOutline],
      inferences: Seq[Inference],
      depth: Int = 0
    ): Proof = {
      ProofOutline(proofSteps)
        .fillIn(ProvingContext.getInitial(premises, Nil, inferences, defaultContext.statementDefinitions, depth))
    }

    def checkProof(
      premises: Seq[PremiseMagnet],
      proofSteps: Seq[StepOutline],
      inferences: Seq[Inference],
      depth: Int = 0
    ) = {
      val proof = prove(premises, proofSteps, inferences, depth)
      val cachedProof = CachedProof(Paths.get(""), premises, proof.steps.map(_.cached))
      cachedProof.steps.matchOutlines(proofSteps) must beTrue
      val serializedProof = cachedProof.serialized
      val deserializedProof = CachedProof.parser(Paths.get("")).parse(Tokenizer.fromString(serializedProof, Paths.get("")))._1
      deserializedProof mustEqual cachedProof
      deserializedProof.validate(ProofEntries(inferences, defaultContext.statementDefinitions)) must beSome(proof)
    }

    "not prove an unfounded statement" in {
      prove(Nil, Seq(φ), Nil) must throwAn[Exception]
    }

    val repetition = axiom("Repetition", Seq(φ), φ)
    val modusPonens = axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
    val implicationIsReflexive = axiom("Implication Is Reflexive", Nil, Implication(φ, φ))
    val extractLeftConjunct = axiom("Extract Left Conjunct", Seq(Conjunction(φ, ψ)), φ, RearrangementType.Simplification)
    val extractRightConjunct = axiom("Extract Left Conjunct", Seq(Conjunction(φ, ψ)), ψ, RearrangementType.Simplification)
    val combineConjunction = axiom("Combine Conjunction", Seq(φ, ψ), Conjunction(φ, ψ), RearrangementType.Expansion)
    val addRightDisjunct = axiom("Add Right Disjunct", Seq(ψ), Disjunction(φ, ψ), RearrangementType.Expansion)
    val combineEquivalence = axiom(
      "Combine Equivalence",
      Seq(Implication(φ, ψ), Implication(ψ, φ)),
      Equivalence(φ, ψ),
      RearrangementType.Expansion)

    val specification = axiom(
      "Specification",
      Seq(ForAll("x")(φ.!(FunctionParameter("x", 0)))),
      φ(a))
    val existence = axiom(
      "Existence",
      Seq(φ(a)),
      Exists("x")(φ.!(FunctionParameter("x", 0))))
    val substitutionOfEquals = axiom(
      "Substitution of Equals",
      Seq(Equals(a, b).elidable, φ(a)),
      φ(b))

    "prove the conclusion of a premiseless inference" in {
      checkProof(
        Nil,
        Seq(Implication(ψ, ψ)),
        Seq(implicationIsReflexive))
    }

    "prove the conclusion of an inference whose premises match proven statements" in {
      checkProof(
        Seq(Implication(Implication(Implication(ψ, χ), ψ), ψ), Implication(Implication(ψ, χ), ψ)),
        Seq(ψ),
        Seq(modusPonens))
    }

    "not prove the conclusion of an inference whose premises can't be matched" in {
      prove(
        Seq(Implication(φ, χ), ψ),
        Seq(φ),
        Seq(modusPonens)
      ) must throwAn[Exception]
    }

    "prove an inference conclusion by simplifying a premise" in {
      val anythingImpliesATrueStatement = axiom(
        "Anything Implies a True Statement",
        Seq(φ),
        Implication(ψ, φ))

      checkProof(
        Seq(Conjunction(φ, ψ)),
        Seq(Implication(χ, φ)),
        Seq(extractLeftConjunct, anythingImpliesATrueStatement))
    }

    "prove a statement by rearranging" in {
      checkProof(
        Seq(Conjunction(φ, ψ), χ),
        Seq(Disjunction(ψ, Conjunction(χ, φ))),
        Seq(extractLeftConjunct, combineConjunction, addRightDisjunct))
    }

    "prove an inference conclusion with a predicate" in {
      checkProof(
        Seq(Equals(a, b), Equals(a, a)),
        Seq(Equals(b, a)),
        Seq(substitutionOfEquals))
    }

    "prove an inference substituting a named predicate for another named predicate" in {
      checkProof(
        Seq(Equals(b, a), φ(b)),
        Seq(φ(a)),
        Seq(substitutionOfEquals))
    }

    "prove an inference with a scoped variable" in {
      val equalityIsReflexive = axiom(
        "Equality Is Reflexive",
        Nil,
        Equals(a, a))
      checkProof(
        Nil,
        Seq(StepOutline.ScopedVariable("y", Seq(Equals.!(FunctionParameter("y", 0), FunctionParameter("y", 0))), None)),
        Seq(equalityIsReflexive))
    }

    "prove a conclusion containing a bound variable" in {
      val equivalenceOfSubstitutedEquals = axiom(
        "Equivalence of Substituted Equals",
        Seq(Equals(a, b)),
        Equivalence(φ(a), φ(b)))
      checkProof(
        Seq(Equals(b, c)),
        Seq(
          StepOutline.ScopedVariable(
            "x",
            Seq(Equivalence.!(
              ElementOf.!(FunctionParameter("x", 0), b.^),
              ElementOf.!(FunctionParameter("x", 0), c.^))),
            None)),
        Seq(equivalenceOfSubstitutedEquals))
    }

    "prove a transformed inference" in {
      checkProof(
        Seq(
          ForAll("x")(φ.!(FunctionParameter("x", 0))),
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.!(FunctionParameter("x", 0))))),
        Seq(
          ForAll("x")(ψ.!(FunctionParameter("x", 0)))),
        Seq(specification, modusPonens))
    }

    "prove a partially transformed inference" in {
      checkProof(
        Seq(
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.!(FunctionParameter("x", 0)))),
          φ(a)),
        Seq(ψ(a)),
        Seq(specification, modusPonens))
    }

    "apply a transformation with applied and unapplied statements" in {
      val modusTollens = axiom("Modus Tollens", Seq(Implication(φ, ψ), Negation(ψ)), Negation(φ))
      checkProof(
        Seq(
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.^)),
          Negation(ψ)),
        Seq(
          StepOutline.ScopedVariable("x", Seq(Negation.!(φ.!(FunctionParameter("x", 0)))), None)),
        Seq(specification, modusTollens))
    }

    "prove a naming step" in {
      val valueForExistence = axiom(
        "Value for Existence",
        Seq(
          Exists("x")(φ.!(FunctionParameter("x", 0))),
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.^))),
        ψ)
      checkProof(
        Seq(
          Exists("x")(Conjunction.!(φ.!(FunctionParameter("x", 0)), ψ.^)),
          ForAll("x")(φ.!(FunctionParameter("x", 0)))),
        Seq(StepOutline.Naming(
          "a",
          Conjunction.!(φ.!(FunctionParameter("a", 0)), ψ.^),
          Seq(ψ.^),
          None)),
        Seq(extractRightConjunct, valueForExistence))
    }

    "prove with an elided premise" in {
      val repeatedPairIsSingleton = axiom(
        "Repeated Pair Is Singleton",
        Nil,
        Equals(Pair(a, a), Singleton(a)))
      checkProof(
        Seq(Equals(b, Pair(a,a))),
        Seq(Equals(b, Singleton(a)) via Equals(Pair(a, a), Singleton(a))),
        Seq(repeatedPairIsSingleton, substitutionOfEquals))
    }

    "prove by expanding a premise" in {
      checkProof(
        Seq(ElementOf(a, b), ElementOf(b, Pair(b, c))),
        Seq(Exists("x")(Conjunction.!(
          ElementOf.!(a.^, FunctionParameter("x", 0)),
          ElementOf.!(FunctionParameter("x", 0), Pair.!(b.^, c.^))))),
        Seq(combineConjunction, existence))
    }

    "prove by doubly-expanding a premise" in {
      checkProof(
        Seq(ElementOf(a, b), ElementOf(b, Pair(b, c)), φ),
        Seq(Exists("x")(Conjunction.!(
          Conjunction.!(
            ElementOf.!(a.^, FunctionParameter("x", 0)),
            φ.^),
          ElementOf.!(FunctionParameter("x", 0), Pair.!(b.^, c.^))))),
        Seq(combineConjunction, existence))
    }

    "prove by expanding with a transform" in {
      val uniqueValueForExistence = axiom(
        "Unique Value for Existence",
        Seq(ForAll("x")(Equivalence.!(φ.!(FunctionParameter("x", 0)), Equals.!(FunctionParameter("x", 0), a.^)))),
        ExistsUnique("x")(φ.!(FunctionParameter("x", 0))))
      checkProof(
        Seq(
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0, 1)), Equals.!(FunctionParameter("x", 0, 1), b.^))),
          ForAll("x")(Implication.!(Equals.!(FunctionParameter("x", 0, 1), b.^), φ.!(FunctionParameter("x", 0, 1))))),
        Seq(ExistsUnique("x")(φ.!(FunctionParameter("x", 0, 1)))),
        Seq(uniqueValueForExistence, combineEquivalence, specification))
    }
  }
}
