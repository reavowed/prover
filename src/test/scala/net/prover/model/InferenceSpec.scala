package net.prover.model

import org.specs2.mutable.Specification
import TestDefinitions._

class InferenceSpec extends Specification {
  "inference id" should {
    "be independent of inference name" in {
      createInference("XXX", Seq(φ), ψ).id mustEqual createInference("YYY", Seq(φ), ψ).id
    }
    "be independent of bound variable names" in {
      createInference("XXX", Nil, ForAll("x")(φ($))).id mustEqual createInference("YYY", Nil, ForAll("x")(φ($))).id
    }
    "be independent of variable names" in {
      createInference("XXX", Seq(φ), ψ(a)).id mustEqual createInference("YYY", Seq(ψ), φ(b)).id
    }
  }
}
