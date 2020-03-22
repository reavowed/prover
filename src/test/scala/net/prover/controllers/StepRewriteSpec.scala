package net.prover.controllers

import net.prover.controllers.StepRewriteController.{InferenceRewritePath, InferenceRewriteSuggestion}
import net.prover.controllers.models.{PathData, RewriteRequest}
import net.prover.model.TestDefinitions._
import net.prover.model.proof.{Step, StepProvingContext}
import org.springframework.http.ResponseEntity

import scala.util.Success

class StepRewriteSpec extends ControllerSpec {

  "proving a step" should {
    "get rewrite suggestions using a generalized deduction premise" in {
      val service = mock[BookService]
      val controller = new StepRewriteController(service)

      val statement = ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals($, Zero)))
      implicit val stepContext = createOuterStepContext(Nil, statement.requiredSubstitutions.terms.map(_._1))

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

      responseEntity.getBody.asInstanceOf[Seq[InferenceRewriteSuggestion]] must contain { (inferenceSuggestion: InferenceRewriteSuggestion) =>
        inferenceSuggestion.inference mustEqual elementOfCartesianProductFromCoordinates
        inferenceSuggestion.rewriteSuggestions must contain { (rewritePath: InferenceRewritePath) =>
          rewritePath.path mustEqual Seq(0, 1, 0)
        }
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
        Seq(Seq(RewriteRequest(Seq(0, 1, 0), Some(elementOfCartesianProductFromCoordinates.id), None, false))))

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
                  assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                  assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))),
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
          RewriteRequest(Seq(0, 1, 0, 1, 0, 0), Some(elementOfCartesianProductFromCoordinates.id), None, false),
          RewriteRequest(Seq(0, 1, 0, 1, 0, 1), Some(elementOfCartesianProductFromCoordinates.id), None, false))))

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
                    assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                    assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))),
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
                        assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, C, D)),
                        assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))),
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
        Seq(Seq(RewriteRequest(Seq(0, 1, 0), Some(elementOfCartesianProductFromCoordinates.id), None, false))))

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
          RewriteRequest(Seq(0, 1, 0, 1, 0, 0), Some(elementOfCartesianProductFromCoordinates.id), None, false),
          RewriteRequest(Seq(0, 1, 0, 1, 0, 1), Some(elementOfCartesianProductFromCoordinates.id), None, false))))

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
          RewriteRequest(Seq(0, 1, 0, 0), Some(elementOfCartesianProductFromCoordinates.id), None, false))))

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
                  assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                  assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))),
                  assertion(substitutionOfEquals, Seq(Implication(Equals($.^, Zero), φ)), Seq(Pair(First($), Second($)), $)))))))))))
    }
  }
}
