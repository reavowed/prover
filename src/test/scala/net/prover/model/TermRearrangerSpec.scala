package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext, TermRearranger}
import org.specs2.mutable.Specification

class TermRearrangerSpec extends Specification {

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

    "rearrange inside equivalence" in {
      val conclusion = Equivalence(Equals(add(add(a, b), add(c, d)), add(c, d)), Equals(add(add(a, c), add(b, d)), add(d, c)))
      testRearranging(conclusion, Nil)
    }

    "rearrange using two operators at once" in {
      // ab + (c + d) = (c + ba) + d
      val conclusion = Equals(add(multiply(a, b), add(c, d)), add(add(c, multiply(b, a)), d))
      testRearranging(conclusion, Nil)
    }

    "rearrange using left distributivity on main LHS" in {
      // a(b + c) = ca + ab
      testRearranging(Equals(multiply(a, add(b, c)), add(multiply(c, a), multiply(a, b))), Nil)
    }

    "rearrange using right distributivity on main LHS" in {
      // (b + c)a = ca + ab
      testRearranging(Equals(multiply(add(b, c), a), add(multiply(c, a), multiply(a, b))), Nil)
    }

    "rearrange using left distributivity on main RHS" in {
      // ca + ab = a(b + c)
      testRearranging(Equals(add(multiply(c, a), multiply(a, b)), multiply(a, add(b, c))), Nil)
    }

    "rearrange using right distributivity on main RHS" in {
      // ca + ab = (b + c)a
      testRearranging(Equals(add(multiply(c, a), multiply(a, b)), multiply(add(b, c), a)), Nil)
    }

    "rearrange using left distributivity on inner LHS" in {
      // a(b + c) + de = (ca + ab) + ed
      testRearranging(Equals(add(multiply(a, add(b, c)), multiply(d, e)), add(add(multiply(c, a), multiply(a, b)), multiply(e, d))), Nil)
    }

    "rearrange using right distributivity on inner LHS" in {
      // (b + c)a + de = ca + ab + ed
      testRearranging(Equals(add(multiply(add(b, c), a), multiply(d, e)), add(add(multiply(c, a), multiply(a, b)), multiply(e, d))), Nil)
    }

    "rearrange using left distributivity on inner RHS" in {
      // ca + ab + de = a(b + c) + ed
      testRearranging(Equals(add(add(multiply(c, a), multiply(a, b)), multiply(d, e)), add(multiply(a, add(b, c)), multiply(e, d))), Nil)
    }

    "rearrange using right distributivity on inner RHS" in {
      // ca + ab + de = (b + c)a + ed
      testRearranging(Equals(add(add(multiply(c, a), multiply(a, b)), multiply(d, e)), add(multiply(add(b, c), a), multiply(e, d))), Nil)
    }

    "rearrange using multiple distributivities" in {
      // a(bc) + d(ec + f) = (ab + de)c + df
      testRearranging(Equals(add(multiply(a, multiply(b, c)), multiply(d, add(multiply(e, c), f))), add(multiply(add(multiply(a, b), multiply(d, e)), c), multiply(d, f))), Nil)
    }

    "rearrange using multiple distributivities the other way" in {
      // (f + ce)d + (cb)a = fd + c(ba + ed)
      // i.e. the above but backwards
      testRearranging(Equals(add(multiply(add(f, multiply(c, e)), d), multiply(multiply(c, b), a)), add(multiply(f, d), multiply(c, add(multiply(b, a), multiply(e, d))))), Nil)
    }
  }
}
