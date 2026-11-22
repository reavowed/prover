package net.prover.proving.derivation

import net.prover.{StepBuilderHelper, StepHelpers}
import net.prover.model.*
import net.prover.model.TestDefinitions.{a, b, *}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext, SubstitutionContext}
import net.prover.proving.extraction.{AppliedExtraction, AppliedInferenceExtraction}
import net.prover.proving.premiseFinding.DerivationFinder
import net.prover.proving.rewrite.RewritePremise
import net.prover.theorems.RecalculateReferences
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class DerivedStatementSerializationSpec extends Specification with StepBuilderHelper with StepHelpers {
  given availableEntries: AvailableEntries = defaultAvailableEntries
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1, χ -> 0), Seq(a -> 0, b -> 0, c -> 0, d -> 0))

  /**
    * Saves the given derivation, with its references calculated, and checks that parsing it gives the same derivation.
    */
  def checkRoundTrip(derivation: DerivedStatement, premises: Seq[Statement])(using VariableDefinitions): Result = {
    given outerStepContext: StepContext = createBaseStepContext(premises)
    val stepWithContext = createTargetStepWithContext(derivation.statement)
    val stepContext = stepWithContext.stepContext
    val premise = RecalculateReferences.updateRewritePremise(RewritePremise.Known(derivation), stepContext, stepWithContext.proofWithContext).get._1
    val serialized = premise.serializedLines.mkString("\n")
    val reparsed = RewritePremise.parser(using stepContext, availableEntriesToProvingContext).parseFromString(serialized, "rewrite premise")
    (reparsed must beEqualTo(premise)) and (reparsed.toProofSteps must beEqualTo(premise.toProofSteps))
  }

  /**
    * Finds a derivation of the target from the premises, and checks that it can be saved (with all references
    * resolved) and parsed back unchanged.
    */
  def roundTrip(target: Statement, premises: Seq[Statement])(check: DerivedStatement => Result)(using VariableDefinitions): Result = {
    given outerStepContext: StepContext = createBaseStepContext(premises)
    val derivation = DerivationFinder.findDerivationForUnwrappedStatement(target).get
    val stepWithContext = createTargetStepWithContext(target)
    val premise = RecalculateReferences.updateRewritePremise(RewritePremise.Known(derivation), stepWithContext.stepContext, stepWithContext.proofWithContext).get._1
    (premise.serializedLines.mkString("\n") must not(contain("???"))) and
      checkRoundTrip(derivation, premises) and
      check(derivation)
  }

  "derived statement serialization" should {
    "round trip a reversal" in {
      roundTrip(Equals(a, b), Seq(Equals(b, a))) { derivation =>
        (derivation must beLike[DerivedStatement] { case DerivedStatement.Reversed(DerivedStatement.Known(_), _) => ok }) and
          (derivation.primaryInference must beSome(reverseEquality.summary))
      }
    }

    "round trip a simplification" in {
      roundTrip(ElementOf(First(a), b), Seq(ElementOf(a, Product(b, c)))) { derivation =>
        (derivation must beLike[DerivedStatement] { case DerivedStatement.Simplified(DerivedStatement.Known(_), _) => ok }) and
          (derivation.primaryInference must beSome(firstCoordinateOfElementOfCartesianProduct.summary))
      }
    }

    "round trip a simplification of a simplified premise" in {
      roundTrip(ElementOf(First(a), b), Seq(Conjunction(ElementOf(a, Product(b, c)), χ))) { derivation =>
        derivation must beLike[DerivedStatement] { case DerivedStatement.Simplified(DerivedStatement.Known(_), _) => ok }
      }
    }

    "round trip an extraction" in {
      roundTrip(Implication(χ, ψ(a)), Seq(Equivalence(χ, ψ(a)))) { derivation =>
        (derivation must beLike[DerivedStatement] { case DerivedStatement.Extracted(DerivedStatement.Known(_), _) => ok }) and
          (derivation.primaryInference must beSome(forwardImplicationFromEquivalence.summary))
      }
    }

    "round trip a composition" in {
      roundTrip(ElementOf(add(First(a), Second(a)), Naturals), Seq(ElementOf(a, Product(Naturals, Naturals)))) { derivation =>
        derivation must beLike[DerivedStatement] { case DerivedStatement.Composed(Seq(_: DerivedStatement.Simplified, _: DerivedStatement.Simplified), _) => ok }
      }
    }

    "round trip a combination with a fact" in {
      roundTrip(Conjunction(ElementOf(Zero, Naturals), χ), Seq(χ)) { derivation =>
        (derivation must beLike[DerivedStatement] { case DerivedStatement.Combined(Seq(_: DerivedStatement.Asserted, DerivedStatement.Known(_)), _) => ok }) and
          (derivation.primaryInference must beNone)
      }
    }

    "round trip a combination whose components share a step" in {
      roundTrip(Conjunction(ElementOf(First(a), b), ElementOf(First(a), b)), Seq(ElementOf(a, Product(b, c)))) { derivation =>
        (derivation must beLike[DerivedStatement] { case DerivedStatement.Combined(Seq(_: DerivedStatement.Simplified, _: DerivedStatement.Simplified), _) => ok }) and
          (derivation.allDerivationSteps must haveSize(3)) and
          (derivation.derivationSteps must haveSize(2))
      }
    }

    "round trip a substitution using an extraction with a reversal grouped into a definition deconstruction" in {
      val f = TermVariablePlaceholder("f", 4)
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1, χ -> 0), Seq(a -> 0, b -> 0, c -> 0, d -> 0, f -> 0))
      roundTrip(ElementOf(c, Domain(f)), Seq(ElementOf(c, a), Conjunction(Function(f), FunctionFrom(f, a, b)))) { derivation =>
        (derivation must beLike[DerivedStatement] {
          case DerivedStatement.Substituted(DerivedStatement.Known(_), DerivedStatement.Extracted(DerivedStatement.Known(_), extraction), _) =>
            (extraction.endsInReversal must beTrue) and (extraction.reversalStep.map(_.inference) must beSome(reverseEquality.summary))
        }) and
          (derivation.primaryInference must beSome(FunctionFrom.deconstructionInference.summary))
      }
    }

    "round trip an assertion" in {
      roundTrip(Equals(a, a), Nil) { derivation =>
        (derivation must beLike[DerivedStatement] { case _: DerivedStatement.Asserted => ok }) and
          (derivation.primaryInference must beSome(equalityIsReflexive.summary))
      }
    }

    "round trip a relation rewrite with a supporting premise" in {
      given SubstitutionContext = SubstitutionContext.outsideProof
      val derivation = DerivedStatement.RelationRewritten(
        DerivedStatement.Known(ElementOf(a, Product(b, c))),
        Some(DerivedStatement.Known(χ)),
        AppliedInferenceExtraction(assertion(firstCoordinateOfElementOfCartesianProduct, Nil, Seq(a, b, c))(summon), AppliedExtraction.empty))
      checkRoundTrip(derivation, Seq(ElementOf(a, Product(b, c)), χ))
    }

    "round trip a substitution using a chain of equalities" in {
      given SubstitutionContext = SubstitutionContext.outsideProof
      val derivation = DerivedStatement.Substituted(
        DerivedStatement.Known(φ(a)),
        DerivedStatement.Transitive(
          DerivedStatement.Known(Equals(a, b)),
          DerivedStatement.Known(Equals(b, c)),
          assertion(equalityIsTransitive, Nil, Seq(a, b, c))(summon)),
        assertion(substitutionOfEquals, Seq(φ($)), Seq(a, c))(summon))
      checkRoundTrip(derivation, Seq(φ(a), Equals(a, b), Equals(b, c)))
    }
  }
}
