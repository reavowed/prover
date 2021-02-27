package net.prover.core.proof

import org.specs2.mutable.Specification
import net.prover.core.CoreDefinitions._
import net.prover.core.substitutions.Substitutions

class ProofValidatorSpec extends Specification {
  "proof validator" should {
    "validate a proof with simple application step" in {
      ProofValidator.validate(
        Seq(Implies(χ, φ), χ),
        φ,
        Seq(RuleOfInferenceApplicationStep(φ, ModusPonens, Substitutions(Seq(χ, φ), Nil))),
        Seq(ModusPonens)) must beSuccessfulTry
    }
    "not validate a proof with incorrect rule application step" in {
      ProofValidator.validate(
        Seq(Implies(φ, ψ), ψ),
        ψ,
        Seq(RuleOfInferenceApplicationStep(ψ, ModusPonens, Substitutions(Seq(φ, ψ), Nil))),
        Seq(ModusPonens)) must beFailedTry
    }
    "not validate a proof with incorrect conclusion" in {
      ProofValidator.validate(
        Seq(Implies(φ, ψ), φ),
        χ,
        Seq(RuleOfInferenceApplicationStep(ψ, ModusPonens, Substitutions(Seq(φ, ψ), Nil))),
        Seq(ModusPonens)) must beFailedTry
    }
    "validate a proof with deduction and application steps" in {
      ProofValidator.validate(
        Seq(Implies(φ, ψ), Implies(ψ, χ)),
        Implies(φ, χ),
        Seq(
          implicationStep(φ,
            RuleOfInferenceApplicationStep(ψ, ModusPonens, Substitutions(Seq(φ, ψ), Nil)),
            RuleOfInferenceApplicationStep(χ, ModusPonens, Substitutions(Seq(ψ, χ), Nil)))),
        Seq(ModusPonens)) must beSuccessfulTry
    }
  }
}
