package net.prover.controllers

import net.prover.controllers.StepRewriteController.{InferenceRewriteSuggestion, PremiseRewritePath, PremiseSuggestion}
import net.prover.controllers.models.{PathData, PremiseRewrite}
import net.prover.model.TestDefinitions._
import net.prover.model.proof.{Step, StepReference}
import net.prover.model.{TermVariablePlaceholder, TestDefinitions}
import org.springframework.http.ResponseEntity

import scala.util.Success

class StepRewriteSpec extends ControllerSpec {

  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name

  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 0), Seq(a -> 0, b -> 0, c -> 0, d -> 0))

  "proving a step" should {
    "rewrite target using a direct premise" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController

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
      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController

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
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepRewriteController

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
        fillerSteps(stepIndex - 2) :+ target(equalityPremise) :+ target(premiseToRewrite) :+ target(ψ),
        fillerSteps(stepIndex - 2) :+ target(equalityPremise) :+ target(premiseToRewrite) :+
          assertion(substitutionOfEquals, Seq(φ($)), Seq(a, b)) :+
          target(ψ))
    }

    "rewrite premise using a reversed premise" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepRewriteController

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
        fillerSteps(stepIndex - 2) :+ target(equalityPremise) :+ target(premiseToRewrite) :+ target(ψ),
        fillerSteps(stepIndex - 2) :+ target(equalityPremise) :+ target(premiseToRewrite) :+
          elided(substitutionOfEquals, Seq(
            assertion(reverseEquality, Nil, Seq(a, b)),
            assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a)))) :+
          target(ψ))
    }

    "rewrite left using a direct premise" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

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
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

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
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

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
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

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
      val A = TermVariablePlaceholder("A", 0)
      val B = TermVariablePlaceholder("B", 1)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(A -> 0, B -> 0))

      implicit val service = mock[BookService]
      val controller = new StepRewriteController

      val statement = ForAll("x")(Implication(ElementOf($, Product(A, B)), Equals($, Zero)))
      implicit val stepContext = createOuterStepContext(Nil)

      service.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns
       Success(createTargetStepWithContext(statement))

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
      val A = TermVariablePlaceholder("A", 0)
      val B = TermVariablePlaceholder("B", 1)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(A -> 0, B -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController

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
                  inferenceExtraction(Seq(
                    assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, A, B)),
                    assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))))),
                  assertion(substitutionOfEquals, Seq(Equals($.^, Zero)), Seq(Pair(First($), Second($)), $)))))))))))
    }

    "apply multiple rewrites using a generalized deduction premise at depth" in {
      val A = TermVariablePlaceholder("A", 0)
      val B = TermVariablePlaceholder("B", 1)
      val C = TermVariablePlaceholder("C", 2)
      val D = TermVariablePlaceholder("D", 3)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(A -> 0, B -> 0, C -> 0, D -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController

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
                    inferenceExtraction(Seq(
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
                        inferenceExtraction(Seq(
                          assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, C, D)),
                          assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))))),
                        assertion(substitutionOfEquals, Seq(Equals(Pair($.^, $.^^^), $.^^)), Seq(Pair(First($), Second($)), $)))))))))))))))),
        Seq("f"))
    }

    "apply rewrite to chained statement using a generalized deduction premise" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0), Seq(a -> 0, b -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

      val statement = Equivalence(ForAll("x")(Implication(ElementOf($, Product(a, b)), Equals($, Zero))), φ)

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
              deduction(ElementOf($, Product(a, b)), Seq(
                elided(elementOfCartesianProductFromCoordinates, Seq(
                  assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, a, b)),
                  assertion(equivalenceOfSubstitutedEquals, Seq(Equals($.^, Zero)), Seq($, Pair(First($), Second($)))))))),
              assertion(distributeImplicationOverEquivalence, Seq(ElementOf($, Product(a, b)), Equals($, Zero), Equals(Pair(First($), Second($)), Zero)), Nil))),
            assertion(distributeUniversalQuantifierOverEquivalence, Seq(Implication(ElementOf($, Product(a, b)), Equals($, Zero)), Implication(ElementOf($, Product(a, b)), Equals(Pair(First($), Second($)), Zero))), Nil))) :+
          target(Equivalence(ForAll("x")(Implication(ElementOf($, Product(a, b)), Equals(Pair(First($), Second($)), Zero))), φ)) :+
          assertion(
            equivalenceIsTransitive,
            Seq(
              ForAll("x")(Implication(ElementOf($, Product(a, b)), Equals($, Zero))),
              ForAll("x")(Implication(ElementOf($, Product(a, b)), Equals(Pair(First($), Second($)), Zero))),
              φ),
            Nil))
    }

    "apply multiple rewrites to chained statement using a generalized deduction premise at depth" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0), Seq(a -> 0, b -> 0, c -> 0, d -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

      val statement = Equivalence(ForAll("x")(Implication(ElementOf($, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair($.^, $), $.^^))))), φ)

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
                deduction(ElementOf($, Product(a, b)), Seq(
                  elided(elementOfCartesianProductFromCoordinates, Seq(
                    assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, a, b)),
                    assertion(equivalenceOfSubstitutedEquals, Seq(ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair($.^^^, $), $.^^)))), Seq($, Pair(First($), Second($)))))))),
                assertion(
                  distributeImplicationOverEquivalence,
                  Seq(
                    ElementOf($, Product(a, b)),
                    ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair($.^, $), $.^^))),
                    ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^), Second($.^)), $), $.^^)))),
                  Nil))),
              assertion(
                distributeUniversalQuantifierOverEquivalence,
                Seq(
                  Implication(ElementOf($.^, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair($.^^, $), $.^)))),
                  Implication(ElementOf($.^, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^^), Second($.^^)), $), $.^))))),
                Nil))),
            elided(elementOfCartesianProductFromCoordinates, Seq(
              generalization("x", Seq(
                deduction(ElementOf($, Product(a, b)), Seq(
                  generalization("y", Seq(
                    deduction(ElementOf($, Product(c, d)), Seq(
                      elided(elementOfCartesianProductFromCoordinates, Seq(
                        assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, c, d)),
                        assertion(equivalenceOfSubstitutedEquals, Seq(Equals(Pair(Pair(First($.^), Second($.^)), $.^^^), $.^^)), Seq($, Pair(First($), Second($)))))))),
                    assertion(
                      distributeImplicationOverEquivalence,
                      Seq(
                        ElementOf($, Product(c, d)),
                        Equals(Pair(Pair(First($.^), Second($.^)), $), $.^^),
                        Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^)),
                      Nil))),
                  assertion(
                    distributeUniversalQuantifierOverEquivalence,
                    Seq(
                      Implication(ElementOf($.^^, Product(c, d)), Equals(Pair(Pair(First($), Second($)), $.^^), $.^)),
                      Implication(ElementOf($.^^, Product(c, d)), Equals(Pair(Pair(First($), Second($)), Pair(First($.^^), Second($.^^))), $.^))),
                    Nil))),
                assertion(
                  distributeImplicationOverEquivalence,
                  Seq(
                    ElementOf($, Product(a, b)),
                    ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^), Second($.^)), $), $.^^))),
                    ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^)))),
                  Nil))),
              assertion(
                distributeUniversalQuantifierOverEquivalence,
                Seq(
                  Implication(ElementOf($.^, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^^), Second($.^^)), $), $.^)))),
                  Implication(ElementOf($.^, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^^), Second($.^^)), Pair(First($), Second($))), $.^))))),
                Nil))),
            assertion(
              equivalenceIsTransitive,
              Seq(
                ForAll("x")(Implication(ElementOf($, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair($.^, $), $.^^))))),
                ForAll("x")(Implication(ElementOf($, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^), Second($.^)), $), $.^^))))),
                ForAll("x")(Implication(ElementOf($, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^)))))),
              Nil))) :+
          target(Equivalence(ForAll("x")(Implication(ElementOf($, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^))))), φ)) :+
          assertion(
            equivalenceIsTransitive,
            Seq(
              ForAll("x")(Implication(ElementOf($, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair($.^, $), $.^^))))),
              ForAll("x")(Implication(ElementOf($, Product(a, b)), ForAll("y")(Implication(ElementOf($, Product(c, d)), Equals(Pair(Pair(First($.^), Second($.^)), Pair(First($), Second($))), $.^^))))),
              φ),
            Nil),
        Seq("f"))
    }

    "rewrite in a deduction antecedent" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0), Seq(a -> 0, b -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController

      val statement = ForAll("x")(Implication(ElementOf($, Product(a, b)), Implication(Equals($, Zero), φ)))

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
          target(ForAll("x")(Implication(ElementOf($, Product(a, b)), Implication(Equals(Pair(First($), Second($)), Zero), φ)))) :+
          elided(elementOfCartesianProductFromCoordinates, Seq(
            generalization("x", Seq(
              deduction(ElementOf($, Product(a, b)), Seq(
                elided("Extracted", Seq(
                  assertion(specification, Seq(Implication(ElementOf($.^, Product(a, b)), Implication(Equals(Pair(First($.^), Second($.^)), Zero), φ))), Seq($)),
                  assertion(modusPonens, Seq(ElementOf($, Product(a, b)), Implication(Equals(Pair(First($), Second($)), Zero), φ)), Nil))),
                elided(elementOfCartesianProductFromCoordinates, Seq(
                  inferenceExtraction(Seq(
                    assertion(elementOfCartesianProductFromCoordinates, Nil, Seq($, a, b)),
                    assertion(reverseEquality, Nil, Seq($, Pair(First($), Second($)))))),
                  assertion(substitutionOfEquals, Seq(Implication(Equals($.^, Zero), φ)), Seq(Pair(First($), Second($)), $)))))))))))
    }

    "correctly increase depth of bound variables when getting rewrite premise suggestions inside a generalization" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 2), Seq(a -> 0))

      implicit val service = mock[BookService]
      val controller = new StepRewriteController

      val premise = Equals($, $.^)
      val premiseReference = StepReference(outerStepPath :+ (stepIndex - 1))
      val target = ForAllIn("x", a)(φ($, $.^))
      implicit val stepContext = createOuterStepContext(Seq("a", "b")).addStatement(premise, premiseReference)

      service.findStep[Step](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns
        Success(createTargetStepWithContext(target))

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
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 2), Seq(a -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)
      val controller = new StepRewriteController

      val boundVariables = Seq("a", "b")
      val premise = Equals($, $.^)
      val premiseReference = StepReference(outerStepPath :+ (stepIndex - 1))
      val statement = ForAllIn("x", a)(φ($, $.^))
      implicit val stepContext = createOuterStepContext(boundVariables).addStatement(premise, premiseReference)

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
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(ForAllIn("x", a)(φ($, $.^^))) :+ elided(substitutionOfEquals, Seq(
          assertion(reverseEquality, Nil, Seq($, $.^)),
          assertion(substitutionOfEquals, Seq(ForAllIn("x", a)(φ($, $.^^^))), Seq($.^, $)))),
        boundVariables)
    }

    "rewrite using an inference extraction with new variables" in {
      val e = TermVariablePlaceholder("e", 4)
      val F = TermVariablePlaceholder("F", 5)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, c -> 0, d -> 0, e -> 0, F -> 2))

      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

      val statement = Equals(F(addZ(a, b), addZ(c, d)), e)
      controller.rewriteLeft(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        Seq(Seq(
          rewrite(integerAdditionIsCommutative, Seq(0), Seq(Commutative.deconstructionInference, extractRightConjunct, specification, modusPonens, specification, modusPonens)),
          rewrite(integerAdditionIsCommutative, Seq(1), Seq(Commutative.deconstructionInference, extractRightConjunct, specification, modusPonens, specification, modusPonens)))))

        checkModifyStepsWithoutProps(
          service,
          fillerSteps(stepIndex - 4) :+ target(ElementOf(a, Integers)) :+ target(ElementOf(b, Integers)) :+ target(ElementOf(c, Integers)) :+ target(ElementOf(d, Integers)) :+ target(statement),
          fillerSteps(stepIndex - 4) :+ target(ElementOf(a, Integers)) :+ target(ElementOf(b, Integers)) :+ target(ElementOf(c, Integers)) :+ target(ElementOf(d, Integers)) :+
            elided(integerAdditionIsCommutative, Seq(
              elided(integerAdditionIsCommutative, Seq(
                elided(integerAdditionIsCommutative, Seq(
                  inferenceExtraction(Seq(
                    assertion(IntegerAdditionDefinition.definitionInference, Nil, Nil),
                    assertion(extractRightConjunct, Seq(BinaryOperation(IntegerAddition), BinaryOperationOn(IntegerAddition, Integers)), Nil),
                    inferenceExtraction(Seq(
                      assertion(BinaryOperationOn.deconstructionInference, Nil, Seq(IntegerAddition, Integers)),
                      assertion(reverseEquality, Nil, Seq(BaseSet(IntegerAddition), Integers)))))),
                  assertion(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(Integers, BaseSet(IntegerAddition))),
                  assertion(substitutionOfEquals, Seq(ElementOf(b, $)), Seq(Integers, BaseSet(IntegerAddition))),
                  inferenceExtraction(Seq(
                    assertion(integerAdditionIsCommutative, Nil, Nil),
                    inferenceExtraction(Seq(
                      assertion(Commutative.deconstructionInference, Nil, Seq(IntegerAddition)),
                      assertion(specification, Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(addZ($.^, $), addZ($, $.^))))), Seq(a)),
                      assertion(modusPonens, Seq(ElementOf(a, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(addZ(a, $), addZ($, a)))), Nil),
                      assertion(specification, Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), Equals(addZ(a, $), addZ($, a)))), Seq(b)),
                      assertion(modusPonens, Seq(ElementOf(b, BaseSet(IntegerAddition)), Equals(addZ(a, b), addZ(b, a))), Nil))))))),
                assertion(substitutionOfEqualsIntoFunction, Nil, Seq(addZ(a, b), addZ(b, a), F($, addZ(c, d)))))),

              elided(integerAdditionIsCommutative, Seq(
                elided(integerAdditionIsCommutative, Seq(
                  inferenceExtraction(Seq(
                    assertion(IntegerAdditionDefinition.definitionInference, Nil, Nil),
                    assertion(extractRightConjunct, Seq(BinaryOperation(IntegerAddition), BinaryOperationOn(IntegerAddition, Integers)), Nil),
                    inferenceExtraction(Seq(
                      assertion(BinaryOperationOn.deconstructionInference, Nil, Seq(IntegerAddition, Integers)),
                      assertion(reverseEquality, Nil, Seq(BaseSet(IntegerAddition), Integers)))))),
                  assertion(substitutionOfEquals, Seq(ElementOf(c, $)), Seq(Integers, BaseSet(IntegerAddition))),
                  assertion(substitutionOfEquals, Seq(ElementOf(d, $)), Seq(Integers, BaseSet(IntegerAddition))),
                  inferenceExtraction(Seq(
                    assertion(integerAdditionIsCommutative, Nil, Nil),
                    inferenceExtraction(Seq(
                      assertion(Commutative.deconstructionInference, Nil, Seq(IntegerAddition)),
                      assertion(specification, Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(addZ($.^, $), addZ($, $.^))))), Seq(c)),
                      assertion(modusPonens, Seq(ElementOf(c, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(addZ(c, $), addZ($, c)))), Nil),
                      assertion(specification, Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), Equals(addZ(c, $), addZ($, c)))), Seq(d)),
                      assertion(modusPonens, Seq(ElementOf(d, BaseSet(IntegerAddition)), Equals(addZ(c, d), addZ(d, c))), Nil))))))),
                assertion(substitutionOfEqualsIntoFunction, Nil, Seq(addZ(c, d), addZ(d, c), F(addZ(b, a), $))))),
              assertion(equalityIsTransitive, Nil, Seq(F(addZ(a, b), addZ(c, d)), F(addZ(b, a), addZ(c, d)), F(addZ(b, a), addZ(d, c)))))) :+
            target(Equals(F(addZ(b, a), addZ(d, c)), e)) :+
            assertion(equalityIsTransitive, Nil, Seq(F(addZ(a, b), addZ(c, d)), F(addZ(b, a), addZ(d, c)), e)))
    }
  }
}
