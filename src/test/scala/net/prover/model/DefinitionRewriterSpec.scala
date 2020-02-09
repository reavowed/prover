package net.prover.model

import net.prover.model.expressions.{FunctionParameter, Statement}
import org.specs2.matcher.MatchResult
import TestDefinitions._
import net.prover.model.proof.{DefinitionRewriter, StepContext}
import org.specs2.mutable.Specification

class DefinitionRewriterSpec extends Specification {

  "rewriting definitions" should {

    def testRewrite(source: Statement, target: Statement, depth: Int = 0): MatchResult[Any] = {
      implicit val stepContext = StepContext.withPremisesAndTerms(Seq(source), Nil).copy(boundVariableLists = (1 to depth).map(i => Seq(s"x_$i")))
      DefinitionRewriter.rewriteDefinitions(source, target) must beSome(beStepThatMakesValidTheorem(Seq(source), target, depth))
    }

    "rewrite basic quantifier equivalence" in {
      "directly from definition in source" in {
        testRewrite(
          Exists("x")(φ(FunctionParameter(0, 0))),
          Negation(ForAll("x")(Negation(φ(FunctionParameter(0, 0))))))
      }

      "directly from definition in target" in {
        testRewrite(
          Negation(ForAll("x")(Negation(φ(FunctionParameter(0, 0))))),
          Exists("x")(φ(FunctionParameter(0, 0))))
      }

      "from definition in source inside negation" in {
        testRewrite(
          Negation(Exists("x")(φ(FunctionParameter(0, 0)))),
          Negation(Negation(ForAll("x")(Negation(φ(FunctionParameter(0, 0)))))))
      }

      "from definition in target inside negation" in {
        testRewrite(
          Negation(Negation(ForAll("x")(Negation(φ(FunctionParameter(0, 0)))))),
          Negation(Exists("x")(φ(FunctionParameter(0, 0)))))
      }

      "removing double negation in source" in {
        testRewrite(
          Negation(Exists("x")(Negation(φ(FunctionParameter(0, 0))))),
          ForAll("x")(φ(FunctionParameter(0, 0))))
      }
      "removing double negation in target" in {
        testRewrite(
          ForAll("x")(φ(FunctionParameter(0, 0))),
          Negation(Exists("x")(Negation(φ(FunctionParameter(0, 0))))))
      }

      "complex rewrite" in {
        testRewrite(
          Exists("x")(Conjunction(φ(FunctionParameter(0, 0)), Negation(ψ(FunctionParameter(0, 0))))),
          Negation(ForAll("x")(Implication(φ(FunctionParameter(0, 0)), ψ(FunctionParameter(0, 0))))))
      }

      "complex rewrite at depth" in {
        testRewrite(
          Exists("x")(Conjunction(φ(FunctionParameter(0, 0), FunctionParameter(0, 1)), Negation(ψ(FunctionParameter(0, 0), FunctionParameter(0, 2))))),
          Negation(ForAll("x")(Implication(φ(FunctionParameter(0, 0), FunctionParameter(0, 1)), ψ(FunctionParameter(0, 0), FunctionParameter(0, 2))))),
          2)
      }
    }
  }
}
