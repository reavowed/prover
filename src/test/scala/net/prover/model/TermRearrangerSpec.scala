package net.prover.model

import net.prover.model.TestDefinitions.{a, b, _}
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Step, TermRearranger}
import net.prover.util.Direction
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments

class TermRearrangerSpec extends Specification {

  implicit val entryContext = defaultEntryContext
  val e = TermVariablePlaceholder("e", 4)
  val f = TermVariablePlaceholder("f", 5)
  implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, c -> 0, d -> 0, e -> 0, f -> 0))

  "rearranging a statement" should {
    def rearrange(targetStatement: Statement, premises: Seq[Statement])(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): Option[Step] = {
      implicit val stepContext = createBaseStepContext(premises, Seq(targetStatement))
      TermRearranger.rearrange(targetStatement)
        .map(_.recalculateReferences(stepContext, implicitly[ProvingContext])._1)
    }

    def testRearranging(targetStatement: Statement, premises: Seq[Statement])(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): MatchResult[Any] = {
      implicit val stepContext = createBaseStepContext(premises, Seq(targetStatement))
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
      val F = TermVariablePlaceholder("F", 4)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, c -> 0, d -> 0, F -> 2))
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

    def testReversableOperationMultipleWays(description: String, f: (Term, Term) => Term, a: Term, b: Term, result: Term, premises: Seq[Statement] = Nil): Fragments = {
      Fragments.foreach(Seq((Direction.Forward, "left"), (Direction.Reverse, "right"))) { case (interiorDirection, directionDescription) =>
        Fragments.foreach(Seq((Direction.Forward, "LHS"), (Direction.Reverse, "RHS"))) { case (sideDirection, sideDescription) =>
          Fragments.foreach(Seq[(Term => Term, Term => Term, String)]((identity[Term], identity[Term], "main"), (add(_, multiply(d, e)), add(multiply(e, d), _), "inner"))) { case (sourceWrapper, resultWrapper, wrapperDescription) =>
            s"rearrange using $directionDescription $description on $wrapperDescription $sideDescription" ! {
              val source = f.tupled(interiorDirection.swapSourceAndResult(a, b))
              val statement = (Equals.apply(_: Term, _: Term)).tupled(sideDirection.swapSourceAndResult(sourceWrapper(source), resultWrapper(result)))
              testRearranging(statement, premises)
            }
          }
        }
      }
    }

    testReversableOperationMultipleWays("distributivity", multiply, a, add(b, c), add(multiply(c, a), multiply(a, b)))

    "rearrange using multiple distributivities" in {
      // a(bc) + d(ec + f) = (ab + de)c + df
      testRearranging(Equals(add(multiply(a, multiply(b, c)), multiply(d, add(multiply(e, c), f))), add(multiply(add(multiply(a, b), multiply(d, e)), c), multiply(d, f))), Nil)
    }

    "rearrange using multiple distributivities the other way" in {
      // (f + ce)d + (cb)a = fd + c(ba + ed)
      // i.e. the above but backwards
      testRearranging(Equals(add(multiply(add(f, multiply(c, e)), d), multiply(multiply(c, b), a)), add(multiply(f, d), multiply(c, add(multiply(b, a), multiply(e, d))))), Nil)
    }

    testReversableOperationMultipleWays("identity", multiply, One, a, a)
    testReversableOperationMultipleWays("absorber", multiply, Zero, a, Zero)

    "rearrange using identities and absorbers" in {
      // (a*1 + b*0, c*1) = (a, c)
      testRearranging(Equals(Pair(add(multiply(a, One), multiply(b, Zero)), multiply(c, One)), Pair(a, c)), Nil)
    }

    testReversableOperationMultipleWays("inverse", addZ, a, IntegerNegation(a), toZ(Zero), Seq(ElementOf(a, Integers)))
    testReversableOperationMultipleWays("inverse extraction", multiplyZ, a, IntegerNegation(b), IntegerNegation(multiplyZ(a, b)), Seq(ElementOf(a, Integers), ElementOf(b, Integers)))

    "rearrange using inverse with extraction" in {
      testRearranging(Equals(addZ(multiplyZ(a, b), multiplyZ(b, IntegerNegation(a))), toZ(Zero)), Seq(ElementOf(a, Integers), ElementOf(b, Integers)))
    }
  }
}
