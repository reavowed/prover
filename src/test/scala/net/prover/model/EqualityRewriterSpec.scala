package net.prover.model

import net.prover.{StepBuilderHelper, ContextHelper}
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof._
import org.specs2.execute.Result
import org.specs2.mutable.Specification

class EqualityRewriterSpec extends Specification with StepBuilderHelper {
  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, c -> 0, d -> 0))

  "rewriting a statement" should {
    def testRewrite(premises: Seq[Statement], target: Statement)(implicit availableEntries: AvailableEntries, variableDefinitions: VariableDefinitions): Result = {
      implicit val stepContext = createBaseStepContext(premises)

      val stepOption = EqualityRewriter.rewrite(target)
      stepOption must beSome(beStepThatMakesValidTheorem(premises, target))

      def checkSteps(steps: Seq[Step]): Result = {
        Result.foreach(steps) { step =>
          step.statement must (beNone ^^ ((s: Statement) => Equals(a, a).calculateSubstitutions(s)(SubstitutionContext.outsideProof)))
          checkSteps(step.asOptionalInstanceOf[Step.WithSubsteps].toSeq.flatMap(_.substeps))
        }
      }
      checkSteps(stepOption.toSeq)
    }

    "rewrite with simplification and expansion" in {
      val premise = Equals(Pair(First(First(Pair(Pair(a, b), c))), b), Pair(c, d))
      val target = Equals(Pair(a, b), Pair(First(Pair(c, d)), d))
      testRewrite(Seq(premise), target)
    }

    "rewrite with a premise requiring complicated simplification" in {
      val A = TermVariablePlaceholder("A", 1)
      val B = TermVariablePlaceholder("B", 2)
      val C = TermVariablePlaceholder("C", 3)
      val D = TermVariablePlaceholder("D", 4)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, A -> 0, B -> 0, C -> 0, D -> 0))

      val premise = ElementOf(a, Product(Product(A, B), Product(C, D)))
      val target = Equals(a, Pair(Pair(First(First(a)), Second(First(a))), Pair(First(Second(a)), Second(Second(a)))))
      testRewrite(Seq(premise), target)
    }

    "rewrite inline" in {
      def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))

      val premises = Seq(Equals(a, b), Equals(c, d))
      val target = Equals(add(a, c), add(b, d))
      testRewrite(premises, target)
    }

    "rewrite inline with simplification and known equality" in {
      def add(l: Term, r: Term) = Apply(Addition, Pair(l, r))

      val premises = Seq(Equals(a, b))
      val target = Equals(add(a, First(Pair(c, d))), add(b, c))
      testRewrite(premises, target)
    }

    "rewrite a function application inline" in {
      val F = TermVariablePlaceholder("F", 2)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, F-> 1))
      val premises = Seq(Equals(a, b))
      val target = Equals(F(a), F(b))
      testRewrite(premises, target)
    }
  }
}
