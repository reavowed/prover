package net.prover.proving.premiseFinding

import net.prover.{ContextHelper, StepHelpers}
import net.prover.model.TestDefinitions.{a, b, _}
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import net.prover.model.{AvailableEntries, TestDefinitions}
import org.specs2.mutable.Specification

class DerivationOrTargetFinderSpec extends Specification with ContextHelper with StepHelpers {
  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name
  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Seq(a -> 0, b -> 0, c -> 0, d -> 0))

  "premise finder" should {

    def findPremiseOrTarget(target: Statement, premises: Seq[Statement], depth: Int = 0)(implicit availableEntries: AvailableEntries): (Seq[Step], Seq[Statement]) = {
      implicit val stepContext = createBaseStepContext(premises, depth)
      DerivationOrTargetFinder.findDerivationsOrTargets(Seq(target))
        .mapLeft(ks => ks.flatMap(_.derivation.steps).distinct.map(_.toProofStep))
        .mapRight(_.map(_.statement))
    }

    "only add one half of a conjunction as a target if the other half is present as a premise" in {
      findPremiseOrTarget(
        Conjunction(φ, ψ),
        Seq(φ)
      ) mustEqual (
        Seq(assertion(combineConjunction, Seq(φ, ψ), Nil)(SubstitutionContext.outsideProof)),
        Seq(ψ.toVariable)
      )
    }

    "replace terms in a target using a fact" in {
      val axiom = createInference(
        "Function Properties of Natural Addition",
        Nil,
        Conjunction(
          Function(Addition),
          FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)))

      findPremiseOrTarget(
        ForAllIn("x", Domain(Addition))(ForAllIn("y", Domain(Addition))(φ($.^, $))),
        Nil)(
        defaultAvailableEntriesPlus(axiom)
      ) mustEqual (
        Seq(
          inferenceExtraction(
            assertion(axiom, Nil, Nil),
            Seq(
              inferenceExtraction(
                assertion(FunctionFrom.deconstructionInference, Nil, Seq(Addition, Product(Naturals, Naturals), Naturals)),
                Seq(
                  assertion(reverseEquality, Nil, Seq(Domain(Addition), Product(Naturals, Naturals))))))),
          assertion(substitutionOfEquals, Seq(ForAllIn("x", $.^)(ForAllIn("y", $.^^)(φ($.^, $)))), Seq(Product(Naturals, Naturals), Domain(Addition))))(SubstitutionContext.outsideProof),
        Seq(ForAllIn("x", Product(Naturals, Naturals))(ForAllIn("y", Product(Naturals, Naturals))(φ($.^, $)))))
    }

    "deconstruct a non-type-statement target" in {
      findPremiseOrTarget(Conjunction(φ, ψ), Nil) mustEqual (
        Seq(assertion(combineConjunction, Seq(φ, ψ), Nil)(SubstitutionContext.outsideProof)),
        Seq(φ.toVariable, ψ.toVariable)
      )
    }

    "not deconstruct a type-statement target" in {
      findPremiseOrTarget(Conjunction(Function(a), FunctionFrom(a, b, c)), Nil) mustEqual (Nil, Seq(Conjunction(Function(a), FunctionFrom(a, b, c))))
    }
  }
}
