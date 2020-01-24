package net.prover.model

import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{FunctionParameter, Statement, Term}
import net.prover.model.proof._
import org.specs2.execute.Result
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class ProofHelperSpec extends Specification {

  "extracting a statement" should {
    def createStepContext(premises: Seq[Statement], depth: Int = 0) = StepContext.withPremisesAndTerms(premises, Nil).copy(boundVariableLists = (1 to depth).map(i => Seq(i.toString)))

    def getExtractions(sourceStatement: Statement, targetStatement: Statement, premises: Seq[Statement], depth: Int = 0): Seq[(Step, Seq[Step.Target])] = {
      implicit val stepContext = createStepContext(premises, depth)
      SubstatementExtractor.findByExtracting(sourceStatement, targetStatement)
    }


    def testExtraction(sourceStatement: Statement, targetStatement: Statement, extraPremises: Seq[Statement], depth: Int = 0): MatchResult[Any] = {
      val premises = sourceStatement +: extraPremises
      implicit val stepContext = createStepContext(premises, depth)
      getExtractions(sourceStatement, targetStatement, premises, depth)
          .collect { case (step, Nil) => step }
          .single
          .must(beSome(beStepThatMakesValidTheorem(premises, depth)))
    }

    "find a statement via specification" in {
      testExtraction(
        ForAll("x")(Equals(FunctionParameter(0, 0), b)),
        Equals(a, b),
        Nil)
    }

    "find a statement via specification and modus ponens" in {
      testExtraction(
        ForAll("x")(Implication(ElementOf(FunctionParameter(0, 0), a), Equals(FunctionParameter(0, 0), b))),
        Equals(c, b),
        Seq(ElementOf(c, a)))
    }
    "find a statement via double nested specification and modus ponens" in {
      testExtraction(
        ForAll("x")(Implication(ElementOf(FunctionParameter(0, 0), A),
          ForAll("y")(Implication(ElementOf(FunctionParameter(0, 0), B),
            Equals(FunctionParameter(0, 1), FunctionParameter(0, 0)))))),
        Equals(a, b),
        Seq(
          ElementOf(a, A),
          ElementOf(b, B)))
    }

    "find a statement via modus ponens using a known fact" in {
      testExtraction(
        ForAll("n")(Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))),
        φ(Zero),
        Nil)
    }

    "find a statement via modus ponens using a premise simplification" in {
      testExtraction(
        ForAll("n")(Implication(ElementOf(FunctionParameter(0, 0), Naturals), φ(FunctionParameter(0, 0)))),
        φ(Successor(a)),
        Seq(ElementOf(a, Naturals)))
    }

    "find a statement via modus ponens using a rewrite" in {
      testExtraction(
        ForAll("n")(Equivalence(ψ(FunctionParameter(0, 0)), φ(FunctionParameter(0, 0)))),
        ψ(a),
        Seq(φ(a)))
    }

    "find a statement with a bound variable appearing only in a subsidiary premise" in {
      testExtraction(
        ForAll("x")(Implication(φ(FunctionParameter(0, 0)), φ(b))),
        φ(b),
        Seq(φ(a)))
    }

    "find a statement with a bound variable appearing only in a subsidiary premise that requires simplification" in {
      testExtraction(
        ForAll("a")(ForAll("b")(ForAll("c")(Implication(
          Conjunction(
            φ(FunctionParameter(0, 2), FunctionParameter(0, 1)),
            φ(FunctionParameter(0, 1), FunctionParameter(0, 0))),
          φ(FunctionParameter(0, 2), FunctionParameter(0, 0)))))),
        φ(a, c),
        Seq(φ(a, b), φ(b, c)))
    }

    "not extract statements that are substitution matches but not exact matches" in {
      getExtractions(
        Implication(φ, ElementOf(a, c)),
        ElementOf(a, b),
        Seq(φ)
      ) must beEmpty
      getExtractions(
        φ,
        ElementOf(a, b),
        Nil
      ) must beEmpty
    }

    "not find spurious results with external bound variables" in {
      getExtractions(
        ForAll("y")(Implication(
          φ(FunctionParameter(0, 2) /*n*/, FunctionParameter(0, 0) /*y*/),
          Exists("x")(ψ(FunctionParameter(0, 0) /*x*/, FunctionParameter(0, 3) /*n*/, FunctionParameter(0, 1) /*y*/)))),
        Exists("x")(ψ(FunctionParameter(0, 0) /*x*/, FunctionParameter(0, 2) /*n*/, Successor(FunctionParameter(0, 1))  /*m+*/)),
        Seq(φ(FunctionParameter(0, 1) /*n*/, Successor(FunctionParameter(0, 1)) /*n+*/)),
        2
      ).filter(_._2.isEmpty) must beEmpty
    }

    "find non-spurious result with external bound variables" in {
      testExtraction(
        ForAll("y")(Implication(
          φ(FunctionParameter(0, 2), FunctionParameter(0, 0)),
          Exists("x")(ψ(FunctionParameter(0, 0), FunctionParameter(0, 3), FunctionParameter(0, 1))))),
        Exists("x")(ψ(FunctionParameter(0, 0), FunctionParameter(0, 2), Successor(FunctionParameter(0, 1)))),
        Seq(φ(FunctionParameter(0, 1), Successor(FunctionParameter(0, 0)))),
        2)
    }

    "extract a statement with an external bound variable appearing twice in an internally bound context" in {
      testExtraction(
        ForAll("x")(Implication(
          ψ(FunctionParameter(0, 0)),
          ForAll("y")(φ(FunctionParameter(0, 0), FunctionParameter(0, 1), FunctionParameter(0, 1))))),
        ForAll("y")(φ(FunctionParameter(0, 0), FunctionParameter(0, 1), FunctionParameter(0, 1))),
        Seq(ψ(FunctionParameter(0, 0))),
        1)
    }
  }

  "rearranging a statement" should {
    def rearrange(targetStatement: Statement, premises: Seq[Statement]): Option[Step] = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, Nil)
      TermRearranger.rearrange(targetStatement)
        .map(_.recalculateReferences(stepContext, implicitly[ProvingContext]))
    }

    def testRearranging(targetStatement: Statement, premises: Seq[Statement]) = {
      implicit val stepContext = StepContext.withPremisesAndTerms(premises, Nil)
      val step = rearrange(targetStatement, premises)
      step must beSome(beStepThatMakesValidTheorem(premises))
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
      stepOption must beSome(beStepThatMakesValidTheorem(premises))

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
