package net.prover.controllers

import net.prover.controllers.models.{PathData, RewriteRequest}
import net.prover.model.definitions.{RearrangementStep, Wrapper}
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Term
import net.prover.model.proof._
import net.prover.model.{Inference, ProvingContext}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Try

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepRewriteController @Autowired() (val bookService: BookService) extends BookModification with InferenceSearch with TransitivityEditing {

  case class InferenceRewriteSuggestion(inference: Inference.Summary, rewriteSuggestions: Seq[RewriteSuggestion])
  case class RewriteSuggestion(path: Seq[Int], result: Term, direction: String)

  @GetMapping(value = Array("/rewriteSuggestions"), produces = Array("application/json;charset=UTF-8"))
  def getSuggestions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String,
    @RequestParam("expression") serializedTerm: String,
    @RequestParam("pathsAlreadyRewritten") pathsAlreadyRewrittenText: String
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      provingContext = ProvingContext.forEntry(books, definitions, book, chapter, theorem)
      (_, stepContext) <- findStep[Step](theorem, proofIndex, stepPath)
      stepProvingContext = StepProvingContext(stepContext, provingContext)
      term <- Term.parser(stepProvingContext).parseFromString(serializedTerm, "term").recoverWithBadRequest
    } yield {
      implicit val implicitStepContext: StepContext = stepContext
      val pathsAlreadyRewritten = pathsAlreadyRewrittenText.split(',').filter(_.nonEmpty).map(_.split('.').map(_.toInt))
      val filter = inferenceFilter(searchText)
      val termsFunctionsAndPaths = term.getTerms(stepContext).filter { case (_, _, path) =>
          !pathsAlreadyRewritten.exists(path.startsWith(_))
      }
      val inferences = provingContext.termRewriteInferences.filter { case (i, _, _) => filter(i) }
      inferences
        .mapCollect { case (inference, left, right) =>
          val fromLeft = termsFunctionsAndPaths.mapCollect { case (term, _, path) =>
            for {
              substitutions <- left.calculateSubstitutions(term)
              result <- right.applySubstitutions(substitutions.stripApplications())
              _ <- PremiseFinder.findPremiseSteps(inference.premises, substitutions)(stepProvingContext)
            } yield RewriteSuggestion(path, result, "Left")
          }
          val fromRight = termsFunctionsAndPaths.mapCollect { case (term, _, path) =>
            for {
              substitutions <- right.calculateSubstitutions(term).flatMap(_.confirmTotality)
              result <- left.applySubstitutions(substitutions)
              _ <- PremiseFinder.findPremiseSteps(inference.premises, substitutions)(stepProvingContext)
            } yield RewriteSuggestion(path, result, "Right")
          }
          val suggestions = fromLeft ++ fromRight
          if (suggestions.nonEmpty)
            Some(InferenceRewriteSuggestion(inference.summary, suggestions))
          else
            None
        }
        .sortBy(_.inference.conclusion.complexity)(implicitly[Ordering[Int]].reverse)
        .take(10)
    }).toResponseEntity
  }

  @PostMapping(value = Array("/rewriteLeft"), produces = Array("application/json;charset=UTF-8"))
  def rewriteLeft(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody rewrites: Seq[RewriteRequest]
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, transitivity, targetLhs, targetRhs) =>
      implicit val spc = stepProvingContext
      for {
        equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
        if equality.transitivity == transitivity
        (intermediateTerm, rearrangementSteps, inferences) <- rewrites.foldLeft(Try((targetLhs, Seq.empty[RearrangementStep], Seq.empty[Inference.Summary]))) { case (trySoFar, rewrite) =>
          for {
            (term, rearrangementStepsSoFar, inferencesSoFar) <- trySoFar
            inference <- findInference(rewrite.inferenceId)
            (inferenceLhs, inferenceRhs) <- equality.unapply(inference.conclusion).orBadRequest("Inference conclusion was not equality")
            (source, target) = if (rewrite.direction == "Left") (inferenceLhs, inferenceRhs) else (inferenceRhs, inferenceLhs)
            (targetTerm, function, _) <- term.getTerms(stepProvingContext.stepContext).find(_._3 == rewrite.path).orBadRequest(s"No term at path ${rewrite.path.mkString(".")}")
            substitutions <- source.calculateSubstitutions(targetTerm).orBadRequest("Could not find substitutions")
            result <- target.applySubstitutions(substitutions.stripApplications()).orBadRequest("Could not apply substitutions to target")
            (premiseSteps, premises, finalPossibleSubsitutions) <- PremiseFinder.findPremiseSteps(inference.premises, substitutions).orBadRequest("Could not find premises")
            finalSubstitutions <- finalPossibleSubsitutions.confirmTotality.orBadRequest("Substitutions were not complete")
            substitutedConclusion <- inference.conclusion.applySubstitutions(finalSubstitutions).orBadRequest("Could not apply substitutions to inference conclusion")
            assertionStep = Step.Assertion(substitutedConclusion, inference, premises.map(Premise.Pending), finalSubstitutions)
            elidedStep = Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference).get
            reversalStepOption = if (rewrite.direction == "Right") Some(equality.reversal.assertionStep(targetTerm, result)) else None
            wrapper = Wrapper.fromFunction(function)
            expansionStepOption = equality.expansion.assertionStepIfNecessary(targetTerm, result, wrapper)
            newRearrangementStep = RearrangementStep(wrapper(result), (elidedStep +: reversalStepOption.toSeq) ++ expansionStepOption.toSeq, inference)
          } yield (wrapper(result), rearrangementStepsSoFar :+ newRearrangementStep, inferencesSoFar :+ inference)
        }
        transitivitySteps = equality.addTransitivityToRearrangement(targetLhs, rearrangementSteps)
        step = inferences.distinct.single match {
          case Some(inference) =>
            Step.Elided.ifNecessary(transitivitySteps, inference).get
          case None =>
            Step.Elided.ifNecessary(transitivitySteps, "Rewritten").get
        }
      } yield (step, Step.Target(equality(intermediateTerm, targetRhs)), intermediateTerm, Nil)
    }
  }

  @PostMapping(value = Array("/rewriteRight"), produces = Array("application/json;charset=UTF-8"))
  def rewriteRight(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestBody rewrites: Seq[RewriteRequest]
  ): ResponseEntity[_] = {
    insertTransitivity(bookKey, chapterKey, theoremKey, proofIndex, stepPath) { (stepProvingContext, transitivity, targetLhs, targetRhs) =>
      implicit val spc = stepProvingContext
      for {
        equality <- stepProvingContext.provingContext.equalityOption.orBadRequest("No equality found")
        if equality.transitivity == transitivity
        (intermediateTerm, rearrangementSteps, inferences) <- rewrites.foldRight(Try((targetRhs, Seq.empty[RearrangementStep], Seq.empty[Inference.Summary]))) { case (rewrite, trySoFar) =>
          for {
            (term, rearrangementStepsSoFar, inferencesSoFar) <- trySoFar
            inference <- findInference(rewrite.inferenceId)
            (inferenceLhs, inferenceRhs) <- equality.unapply(inference.conclusion).orBadRequest("Inference conclusion was not equality")
            (source, target) = if (rewrite.direction == "Left") (inferenceLhs, inferenceRhs) else (inferenceRhs, inferenceLhs)
            (targetTerm, function, _) <- term.getTerms(stepProvingContext.stepContext).find(_._3 == rewrite.path).orBadRequest(s"No term at path ${rewrite.path.mkString(".")}")
            substitutions <- source.calculateSubstitutions(targetTerm).orBadRequest("Could not find substitutions")
            result <- target.applySubstitutions(substitutions.stripApplications()).orBadRequest("Could not apply substitutions to target")
            (premiseSteps, premises, finalPossibleSubsitutions) <- PremiseFinder.findPremiseSteps(inference.premises, substitutions).orBadRequest("Could not find premises")
            finalSubstitutions <- finalPossibleSubsitutions.confirmTotality.orBadRequest("Substitutions were not complete")
            substitutedConclusion <- inference.conclusion.applySubstitutions(finalSubstitutions).orBadRequest("Could not apply substitutions to inference conclusion")
            assertionStep = Step.Assertion(substitutedConclusion, inference, premises.map(Premise.Pending), finalSubstitutions)
            elidedStep = Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference).get
            reversalStepOption = if (rewrite.direction == "Left") Some(equality.reversal.assertionStep(result, targetTerm)) else None
            wrapper = Wrapper.fromFunction(function)
            expansionStepOption = equality.expansion.assertionStepIfNecessary(result, targetTerm, wrapper)
            newRearrangementStep = RearrangementStep(wrapper(targetTerm), (elidedStep +: reversalStepOption.toSeq) ++ expansionStepOption.toSeq, inference)
          } yield (wrapper(result), newRearrangementStep +: rearrangementStepsSoFar, inference +: inferencesSoFar)
        }
        transitivitySteps = equality.addTransitivityToRearrangement(intermediateTerm, rearrangementSteps)
        step = inferences.distinct.single match {
          case Some(inference) =>
            Step.Elided.ifNecessary(transitivitySteps, inference).get
          case None =>
            Step.Elided.ifNecessary(transitivitySteps, "Rewritten").get
        }
      } yield (Step.Target(equality(targetLhs, intermediateTerm)), step, intermediateTerm, Nil)
    }
  }
}
