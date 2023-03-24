package net.prover.model

import net.prover.StepContextHelper
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.Statement
import net.prover.model.proof.DefinitionRewriter
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class DefinitionRewriterSpec extends Specification with StepContextHelper {

  implicit val entryContext = defaultEntryContext
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1), Nil)

  "rewriting definitions" should {

    def testRewrite(source: Statement, target: Statement, depth: Int = 0)(implicit variableDefinitions: VariableDefinitions): MatchResult[Any] = {
      implicit val stepContext = createBaseStepContext(Seq(source)).copy(boundVariableLists = (1 to depth).map(i => Seq(s"x_$i")))
      DefinitionRewriter.rewriteDefinitions(source, target) must beSome(beStepThatMakesValidTheorem(Seq(source), target, depth))
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
        implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Nil)
        testRewrite(
          Exists("x")(Conjunction(φ($), Negation(ψ($)))),
          Negation(ForAll("x")(Implication(φ($), ψ($)))))
      }

      "complex rewrite at depth" in {
        implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 2, ψ -> 2), Nil)
        testRewrite(
          Exists("x")(Conjunction(φ($, $.^), Negation(ψ($, $.^^)))),
          Negation(ForAll("x")(Implication(φ($, $.^), ψ($, $.^^)))),
          2)
      }
    }
  }
}
