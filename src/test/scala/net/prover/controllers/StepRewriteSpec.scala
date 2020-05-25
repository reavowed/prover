package net.prover.controllers

import net.prover.controllers.StepRewriteController.{InferenceRewriteSuggestion, PremiseRewritePath, PremiseSuggestion}
import net.prover.controllers.models.{PathData, PremiseRewrite, RewriteRequest}
import net.prover.model.TestDefinitions
import net.prover.model.TestDefinitions.{target, _}
import net.prover.model.proof.{PremiseReference, Step, StepProvingContext, StepReference}
import org.springframework.http.ResponseEntity

import scala.util.Success

class StepRewriteSpec extends ControllerSpec {

  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name

  "proving a step" should {
    "rewrite target using a direct premise" in {
      val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController(service)

      val equalityPremise = Equals(a, b)

      controller.rewriteManually(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(equalityPremise, Seq(0)))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 1) :+ target(equalityPremise) :+ target(φ(a)),
        fillerSteps(stepIndex - 1) :+ target(equalityPremise) :+
          target(φ(b)) :+
          elided(substitutionOfEquals, Seq(
            assertion(reverseEquality, Nil, Seq(a, b)),
            assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a)))))
    }

    "rewrite target using a reversed premise" in {
      val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController(service)

      val equalityPremise = Equals(a, b)

      controller.rewriteManually(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(Equals(b, a), Seq(0)))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 1) :+ target(equalityPremise) :+ target(φ(b)),
        fillerSteps(stepIndex - 1) :+ target(equalityPremise) :+
          target(φ(a)) :+
          assertion(substitutionOfEquals, Seq(φ($)), Seq(a, b)))
    }

    "rewrite premise using a direct premise" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepRewriteController(service)

      val equalityPremise = Equals(a, b)
      val premiseToRewrite = φ(a)

      controller.rewritePremise(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        PremiseRewrite(
          premiseToRewrite.serialized,
          Seq(Seq(rewrite(equalityPremise, Seq(0))))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 2) :+ target(equalityPremise) :+ target(premiseToRewrite) :+ target(φ),
        fillerSteps(stepIndex - 2) :+ target(equalityPremise) :+ target(premiseToRewrite) :+
          assertion(substitutionOfEquals, Seq(φ($)), Seq(a, b)) :+
          target(φ))
    }

    "rewrite premise using a reversed premise" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepRewriteController(service)

      val equalityPremise = Equals(a, b)
      val premiseToRewrite = φ(b)

      controller.rewritePremise(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        PremiseRewrite(
          premiseToRewrite.serialized,
          Seq(Seq(rewrite(Equals(b, a), Seq(0))))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 2) :+ target(equalityPremise) :+ target(premiseToRewrite) :+ target(φ),
        fillerSteps(stepIndex - 2) :+ target(equalityPremise) :+ target(premiseToRewrite) :+
          elided(substitutionOfEquals, Seq(
            assertion(reverseEquality, Nil, Seq(a, b)),
            assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a)))) :+
          target(φ))
    }

    "rewrite left using a direct premise" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController(service)

      val equalityPremise = Equals(b, Zero)

      controller.rewriteLeft(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(equalityPremise, Seq(1, 1)))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 1) :+
          target(equalityPremise) :+
          target(lessThan(add(a, b), c)),
        fillerSteps(stepIndex - 1) :+
          target(equalityPremise) :+
          assertion(substitutionOfEqualsIntoFunction, Nil, Seq(b, Zero, add(a, $))) :+
          target(lessThan(add(a, Zero), c)) :+
          elided(substitutionOfEquals, Seq(
            assertion(reverseEquality, Nil, Seq(add(a, b), add(a, Zero))),
            assertion(substitutionOfEquals, Seq(lessThan($, c)), Seq(add(a, Zero), add(a, b))))))
    }

    "rewrite left using a reversed premise" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController(service)

      val equalityPremise = Equals(Zero, b)

      controller.rewriteLeft(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(Equals(b, Zero), Seq(1, 1)))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 1) :+
          target(equalityPremise) :+
          target(lessThan(add(a, b), c)),
        fillerSteps(stepIndex - 1) :+
          target(equalityPremise) :+
          elided(substitutionOfEqualsIntoFunction, Seq(
            assertion(reverseEquality, Nil, Seq(Zero, b)),
            assertion(substitutionOfEqualsIntoFunction, Nil, Seq(b, Zero, add(a, $))))) :+
          target(lessThan(add(a, Zero), c)) :+
          elided(substitutionOfEquals, Seq(
            assertion(reverseEquality, Nil, Seq(add(a, b), add(a, Zero))),
            assertion(substitutionOfEquals, Seq(lessThan($, c)), Seq(add(a, Zero), add(a, b))))))
    }

    "rewrite right using a direct premise" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController(service)

      val equalityPremise = Equals(c, Zero)

      controller.rewriteRight(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(equalityPremise, Seq(1, 1)))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 1) :+
          target(equalityPremise) :+
          target(lessThan(a, add(b, c))),
        fillerSteps(stepIndex - 1) :+
          target(equalityPremise) :+
          target(lessThan(a, add(b, Zero))) :+
          elided(substitutionOfEqualsIntoFunction, Seq(
            assertion(reverseEquality, Nil, Seq(c, Zero)),
            assertion(substitutionOfEqualsIntoFunction, Nil, Seq(Zero, c, add(b, $))))) :+
          assertion(substitutionOfEquals, Seq(lessThan(a, $)), Seq(add(b, Zero), add(b, c))))
    }

    "rewrite right using a reversed premise" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController(service)

      val equalityPremise = Equals(Zero, c)

      controller.rewriteRight(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(Equals(c, Zero), Seq(1, 1)))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 1) :+
          target(equalityPremise) :+
          target(lessThan(a, add(b, c))),
        fillerSteps(stepIndex - 1) :+
          target(equalityPremise) :+
          target(lessThan(a, add(b, Zero))) :+
          assertion(substitutionOfEqualsIntoFunction, Nil, Seq(Zero, c, add(b, $))) :+
          assertion(substitutionOfEquals, Seq(lessThan(a, $)), Seq(add(b, Zero), add(b, c))))
    }

    "get rewrite suggestions using a generalized deduction premise" in {
      val service = mock[BookService]
      val controller = new StepRewriteController(service)

      val statement = ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals($, Zero)))
      implicit val stepContext = createOuterStepContextForStatements(Seq(statement), Nil)

      service.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns
       Success((Step.Target(statement), implicitly[StepProvingContext]))

      val responseEntity = controller.getSuggestions(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        "",
        statement.serialized,
        ""
      ).asInstanceOf[ResponseEntity[Seq[InferenceRewriteSuggestion]]]

      responseEntity.getBody must beAnInstanceOf[Seq[InferenceRewriteSuggestion]]

      responseEntity.getBody must contain { (inferenceSuggestion: InferenceRewriteSuggestion) =>
        inferenceSuggestion.inference mustEqual elementOfCartesianProductFromCoordinates
        inferenceSuggestion.paths must contain(Seq(0, 1, 0))
      }
    }

    "apply rewrite using a generalized deduction premise" in {
      val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController(service)

      val statement = ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals($, Zero)))

      controller.rewriteManually(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(elementOfCartesianProductFromCoordinates, Seq(0, 1, 0), Nil))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex) :+ target(statement),
        fillerSteps(stepIndex) :+
          target(ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals(Pair(First($), Second($)), Zero)))) :+
          elided(elementOfCartesianProductFromCoordinates, Seq(
            generalization("x", Seq(
              deduction(ElementOf($, Product(A, B)), Seq(
                elided("Extracted", Seq(
                  assertion(specification, Seq(Implication(ElementOf($.^, Product(A, B)), Equals(Pair(First($.^), Second($.^)), Zero))), Seq($)),
                  assertion(modusPonens, Seq(ElementOf($, Product(A, B)), Equals(Pair(First($), Second($)), Zero)), Nil))),
                elided(elementOfCartesianProductFromCoordinates, Seq(
                  elided(elementOfCartesianProductFromCoordinates, Seq(
                    assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                    assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))))),
                  assertion(substitutionOfEquals, Seq(Equals($.^, Zero)), Seq(Pair(First($), Second($)), $)))))))))))
    }

    "apply multiple rewrites using a generalized deduction premise at depth" in {
      val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController(service)

      val statement = ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^, $), $.^^)))))

      controller.rewriteManually(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(
          rewrite(elementOfCartesianProductFromCoordinates, Seq(0, 1, 0, 1, 0, 0), Nil),
          rewrite(elementOfCartesianProductFromCoordinates, Seq(0, 1, 0, 1, 0, 1), Nil))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex) :+ target(statement),
        fillerSteps(stepIndex) :+
          target(ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^)))))) :+
          elided(elementOfCartesianProductFromCoordinates, Seq(
            elided(elementOfCartesianProductFromCoordinates, Seq(
              generalization("x", Seq(
                deduction(ElementOf($, Product(A, B)), Seq(
                  elided("Extracted", Seq(
                    assertion(specification, Seq(Implication(ElementOf($.^^, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^^^), Second($.^^^)), Pair(First($), Second($))), $.^^))))), Seq($)),
                    assertion(modusPonens, Seq(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^)))), Nil))),
                  elided(elementOfCartesianProductFromCoordinates, Seq(
                    elided(elementOfCartesianProductFromCoordinates, Seq(
                      assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                      assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))))),
                    assertion(substitutionOfEquals, Seq(ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^^^, Pair(First($), Second($))), $.^^)))), Seq(Pair(First($), Second($)), $)))))))))),
            elided(elementOfCartesianProductFromCoordinates, Seq(
              generalization("x", Seq(
                deduction(ElementOf($, Product(A, B)), Seq(
                  generalization("y", Seq(
                    deduction(ElementOf($, Product(C, D)), Seq(
                      elided("Extracted", Seq(
                        assertion(specification, Seq(Implication(ElementOf($.^^^, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^^^^, Pair(First($), Second($))), $.^^^))))), Seq($.^)),
                        assertion(modusPonens, Seq(ElementOf($.^, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^^, Pair(First($), Second($))), $.^^^)))), Nil),
                        assertion(specification, Seq(Implication(ElementOf($.^^^, Product(C, D)), Equals(Pair($.^, Pair(First($.^^^), Second($.^^^))), $.^^))), Seq($)),
                        assertion(modusPonens, Seq(ElementOf($, Product(C, D)), Equals(Pair($.^, Pair(First($), Second($))), $.^^)), Nil))),
                      elided(elementOfCartesianProductFromCoordinates, Seq(
                        elided(elementOfCartesianProductFromCoordinates, Seq(
                          assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, C, D)),
                          assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))))),
                        assertion(substitutionOfEquals, Seq(Equals(Pair($.^, $.^^^), $.^^)), Seq(Pair(First($), Second($)), $)))))))))))))))),
        Seq("f"))
    }

    "apply rewrite to chained statement using a generalized deduction premise" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController(service)

      val statement = Equivalence(ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals($, Zero))), φ)

      controller.rewriteLeft(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(elementOfCartesianProductFromCoordinates, Seq(0, 1, 0), Nil))))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(statement),
        fillerSteps(stepIndex) :+
          elided(elementOfCartesianProductFromCoordinates, Seq(
            generalization("x", Seq(
              deduction(ElementOf($, Product(A, B)), Seq(
                elided(elementOfCartesianProductFromCoordinates, Seq(
                  assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                  assertion(equivalenceOfSubstitutedEquals, Seq(Equals($.^, Zero)), Seq($, Pair(First($), Second($)))))))),
              assertion(distributeImplicationOverEquivalence, Seq(ElementOf($, Product(A, B)), Equals($, Zero), Equals(Pair(First($), Second($)), Zero)), Nil))),
            assertion(distributeUniversalQuantifierOverEquivalence, Seq(Implication(ElementOf($, Product(A, B)), Equals($, Zero)), Implication(ElementOf($, Product(A, B)), Equals(Pair(First($), Second($)), Zero))), Nil))) :+
          target(Equivalence(ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals(Pair(First($), Second($)), Zero))), φ)) :+
          assertion(
            equivalenceIsTransitive,
            Seq(
              ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals($, Zero))),
              ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals(Pair(First($), Second($)), Zero))),
              φ),
            Nil))
    }

    "apply multiple rewrites to chained statement using a generalized deduction premise at depth" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController(service)

      val statement = Equivalence(ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^, $), $.^^))))), φ)

      controller.rewriteLeft(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(
          rewrite(elementOfCartesianProductFromCoordinates, Seq(0, 1, 0, 1, 0, 0), Nil),
          rewrite(elementOfCartesianProductFromCoordinates, Seq(0, 1, 0, 1, 0, 1), Nil))))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(statement),
        fillerSteps(stepIndex) :+
          elided(elementOfCartesianProductFromCoordinates, Seq(
            elided(elementOfCartesianProductFromCoordinates, Seq(
              generalization("x", Seq(
                deduction(ElementOf($, Product(A, B)), Seq(
                  elided(elementOfCartesianProductFromCoordinates, Seq(
                    assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                    assertion(equivalenceOfSubstitutedEquals, Seq(ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^^^, $), $.^^)))), Seq($, Pair(First($), Second($)))))))),
                assertion(
                  distributeImplicationOverEquivalence,
                  Seq(
                    ElementOf($, Product(A, B)),
                    ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^, $), $.^^))),
                    ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), $), $.^^)))),
                  Nil))),
              assertion(
                distributeUniversalQuantifierOverEquivalence,
                Seq(
                  Implication(ElementOf($.^, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^^, $), $.^)))),
                  Implication(ElementOf($.^, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^^), Second($.^^)), $), $.^))))),
                Nil))),
            elided(elementOfCartesianProductFromCoordinates, Seq(
              generalization("x", Seq(
                deduction(ElementOf($, Product(A, B)), Seq(
                  generalization("y", Seq(
                    deduction(ElementOf($, Product(C, D)), Seq(
                      elided(elementOfCartesianProductFromCoordinates, Seq(
                        assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, C, D)),
                        assertion(equivalenceOfSubstitutedEquals, Seq(Equals(Pair(Pair(First($.^), Second($.^)), $.^^^), $.^^)), Seq($, Pair(First($), Second($)))))))),
                    assertion(
                      distributeImplicationOverEquivalence,
                      Seq(
                        ElementOf($, Product(C, D)),
                        Equals(Pair(Pair(First($.^), Second($.^)), $), $.^^),
                        Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^)),
                      Nil))),
                  assertion(
                    distributeUniversalQuantifierOverEquivalence,
                    Seq(
                      Implication(ElementOf($.^^, Product(C, D)), Equals(Pair(Pair(First($), Second($)), $.^^), $.^)),
                      Implication(ElementOf($.^^, Product(C, D)), Equals(Pair(Pair(First($), Second($)), Pair(First($.^^), Second($.^^))), $.^))),
                    Nil))),
                assertion(
                  distributeImplicationOverEquivalence,
                  Seq(
                    ElementOf($, Product(A, B)),
                    ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), $), $.^^))),
                    ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^)))),
                  Nil))),
              assertion(
                distributeUniversalQuantifierOverEquivalence,
                Seq(
                  Implication(ElementOf($.^, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^^), Second($.^^)), $), $.^)))),
                  Implication(ElementOf($.^, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^^), Second($.^^)), Pair(First($), Second($))), $.^))))),
                Nil))),
            assertion(
              equivalenceIsTransitive,
              Seq(
                ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^, $), $.^^))))),
                ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), $), $.^^))))),
                ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^)))))),
              Nil))) :+
          target(Equivalence(ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^))))), φ)) :+
          assertion(
            equivalenceIsTransitive,
            Seq(
              ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair($.^, $), $.^^))))),
              ForAll("x")(Implication(ElementOf($, Product(A, B)), ForAll("y")(Implication(ElementOf($, Product(C, D)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^))))),
              φ),
            Nil),
        Seq("f"))
    }

    "rewrite in a deduction antecedent" in {
      val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController(service)

      val statement = ForAll("x")(Implication(ElementOf($, Product(A, B)), Implication(Equals($, Zero), φ)))

      controller.rewriteManually(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(
          rewrite(elementOfCartesianProductFromCoordinates, Seq(0, 1, 0, 0), Nil))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex) :+ target(statement),
        fillerSteps(stepIndex) :+
          target(ForAll("x")(Implication(ElementOf($, Product(A, B)), Implication(Equals(Pair(First($), Second($)), Zero), φ)))) :+
          elided(elementOfCartesianProductFromCoordinates, Seq(
            generalization("x", Seq(
              deduction(ElementOf($, Product(A, B)), Seq(
                elided("Extracted", Seq(
                  assertion(specification, Seq(Implication(ElementOf($.^, Product(A, B)), Implication(Equals(Pair(First($.^), Second($.^)), Zero), φ))), Seq($)),
                  assertion(modusPonens, Seq(ElementOf($, Product(A, B)), Implication(Equals(Pair(First($), Second($)), Zero), φ)), Nil))),
                elided(elementOfCartesianProductFromCoordinates, Seq(
                  elided(elementOfCartesianProductFromCoordinates, Seq(
                    assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                    assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))))),
                  assertion(substitutionOfEquals, Seq(Implication(Equals($.^, Zero), φ)), Seq(Pair(First($), Second($)), $)))))))))))
    }

    "correctly increase depth of bound variables when getting rewrite premise suggestions inside a generalization" in {
      val service = mock[BookService]
      val controller = new StepRewriteController(service)

      val premise = Equals($, $.^)
      val premiseReference = StepReference(outerStepPath :+ (stepIndex - 1))
      val target = ForAllIn("x", A)(φ($, $.^))
      implicit val stepContext = createOuterStepContextForStatements(Seq(premise, target), Seq("a", "b")).addStatement(premise, premiseReference)

      service.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns
        Success((Step.Target(target), implicitly[StepProvingContext]))

      val responseEntity = controller.getRewritePremiseSuggestions(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        target.serialized,
        ""
      ).asInstanceOf[ResponseEntity[Seq[PremiseSuggestion]]]

      responseEntity.getBody must beAnInstanceOf[Seq[PremiseSuggestion]]

      responseEntity.getBody mustEqual Seq(PremiseSuggestion(
        premise,
        Some(premiseReference),
        Seq(PremiseRewritePath(Seq(0, 1, 1), $.^))))
    }

    "correctly increase depth of bound variables when applying rewrite inside a generalization" in {
      val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController(service)

      val boundVariables = Seq("a", "b")
      val premise = Equals($, $.^)
      val premiseReference = StepReference(outerStepPath :+ (stepIndex - 1))
      val statement = ForAllIn("x", A)(φ($, $.^))
      implicit val stepContext = createOuterStepContextForStatements(Seq(premise, statement), boundVariables).addStatement(premise, premiseReference)

      controller.rewriteManually(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(rewrite(premise, Seq(0, 1, 1)))))

      checkModifyStepsWithoutProps(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(statement),
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(ForAllIn("x", A)(φ($, $.^^))) :+ elided(substitutionOfEquals, Seq(
          assertion(reverseEquality, Nil, Seq($, $.^)),
          assertion(substitutionOfEquals, Seq(ForAllIn("x", A)(φ($, $.^^^))), Seq($.^, $)))),
        boundVariables)
    }
  }
}
