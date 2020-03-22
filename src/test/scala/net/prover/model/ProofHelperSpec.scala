package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{FunctionParameter, Statement, Term}
import net.prover.model.proof._
import org.specs2.execute.Result
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class ProofHelperSpec extends Specification {
  "rearranging a statement" should {
    def rearrange(targetStatement: Statement, premises: Seq[Statement]): Option[Step] = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, Nil)
      TermRearranger.rearrange(targetStatement)
        .map(_.recalculateReferences(stepContext, implicitly[ProvingContext])._1)
    }

    def testRearranging(targetStatement: Statement, premises: Seq[Statement]) = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, Nil)
      val step = rearrange(targetStatement, premises)
      step must beSome(beStepThatMakesValidTheorem(premises, targetStatement))
    }

    "rearrange with associativity and commutativity" in {
      val conclusion = Equals(
        add(add(a, b), add(c, d)),
        add(add(a, c), add(b, d)))
      testRearranging(conclusion, Nil)
    }

    "rearrange using a premise in same order" in {
      val premise = Equals(add(a, b), add(c, d))
      val conclusion = Equals(add(d, c), add(b, a))
      testRearranging(conclusion, Seq(premise))
    }

    "rearrange using a premise in reversed order" in {
      val premise = Equals(add(a, b), add(c, d))
      val conclusion = Equals(add(b, a), add(d, c))
      testRearranging(conclusion, Seq(premise))
    }

    "rearrange inside a function" in {
      val conclusion = Equals(F(add(add(a, b), add(c, d)), add(c, d)), F(add(add(a, c), add(b, d)), add(d, c)))
      testRearranging(conclusion, Nil)
    }
  }

  "rewriting a statement" should {

    def testRewrite(premises: Seq[Statement], target: Statement) = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, Nil)

      val stepOption = EqualityRewriter.rewrite(target)
      stepOption must beSome(beStepThatMakesValidTheorem(premises, target))

      def checkSteps(steps: Seq[Step]): Result = {
        Result.foreach(steps) { step =>
          step.provenStatement must beSome(beNone ^^ ((s: Statement) => Equals(a, a).calculateSubstitutions(s)(SubstitutionContext.outsideProof)))
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
      val premises = Seq(Equals(a, b))
      val target = Equals(F(a), F(b))
      testRewrite(premises, target)
    }
  }
}
