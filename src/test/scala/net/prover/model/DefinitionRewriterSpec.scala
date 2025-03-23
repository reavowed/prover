package net.prover.model

import net.prover.model.TestDefinitions.*
import net.prover.model.expressions.Statement
import net.prover.model.proof.{DefinitionRewriter, StepContext}
import net.prover.{ContextHelper, StepBuilderHelper}
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class DefinitionRewriterSpec extends Specification with StepBuilderHelper {

  given availableEntries: AvailableEntries = defaultAvailableEntries
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1), Nil)

  "rewriting definitions" should {

    def testRewrite(source: Statement, target: Statement, depth: Int = 0)(implicit variableDefinitions: VariableDefinitions): Result = {
      given stepContext: StepContext = (1 to depth).foldLeft(createBaseStepContext(Seq(source))) {(c, i) => c.addBoundVariable(s"x_$i") }
      DefinitionRewriter.rewriteDefinitions(source, target) must beSome(beStepThatMakesValidAndCompleteTheorem(Seq(source), target, depth))
    }

    "rewrite basic quantifier equivalence" in {
      "directly from definition in source" in {
        testRewrite(
          Exists("x")(φ($)),
          Negation(ForAll("x")(Negation(φ($)))))
      }

      "directly from definition in target" in {
        testRewrite(
          Negation(ForAll("x")(Negation(φ($)))),
          Exists("x")(φ($)))
      }

      "from definition in source inside negation" in {
        testRewrite(
          Negation(Exists("x")(φ($))),
          Negation(Negation(ForAll("x")(Negation(φ($))))))
      }

      "from definition in target inside negation" in {
        testRewrite(
          Negation(Negation(ForAll("x")(Negation(φ($))))),
          Negation(Exists("x")(φ($))))
      }

      "removing double negation in source" in {
        testRewrite(
          Negation(Exists("x")(Negation(φ($)))),
          ForAll("x")(φ($)))
      }
      "removing double negation in target" in {
        testRewrite(
          ForAll("x")(φ($)),
          Negation(Exists("x")(Negation(φ($)))))
      }

      "complex rewrite" in {
        given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Nil)
        testRewrite(
          Exists("x")(Conjunction(φ($), Negation(ψ($)))),
          Negation(ForAll("x")(Implication(φ($), ψ($)))))
      }

      "complex rewrite at depth" in {
        given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 2, ψ -> 2), Nil)
        testRewrite(
          Exists("x")(Conjunction(φ($, $.^), Negation(ψ($, $.^^)))),
          Negation(ForAll("x")(Implication(φ($, $.^), ψ($, $.^^)))),
          2)
      }
    }
  }
}
