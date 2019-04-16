package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model._
import net.prover.model.proof.{PremiseContext, ProofHelper, Step, StepContext}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/{stepPath}"))
class StepSuggestionController @Autowired() (val bookService: BookService) extends BookModification {

  case class InferenceSuggestion(
    inference: Inference.Summary,
    requiredSubstitutions: Substitutions.Required,
    substitutions: Option[Seq[Substitutions]])
  @GetMapping(value = Array("/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String,
    @RequestParam("withConclusion") withConclusion: Boolean
  ): ResponseEntity[_] = {
    val searchWords = searchText.toLowerCase().splitByWhitespace()
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      entryContext = EntryContext.forEntry(books, book, chapter, theorem)
      (step, stepContext, _) <- findStep[Step](theorem, stepPath, entryContext)
      getSubstitutions <- if (withConclusion)
        step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target").map { targetStep =>
          (inference: Inference) =>
            val substitutions = inference.conclusion.calculateSubstitutions(targetStep.statement, Substitutions.empty, 0, stepContext.externalDepth)
            if (substitutions.nonEmpty) Some(Some(substitutions)) else None
        }
        else
         Success((_: Inference) => Some(None))
    } yield {
      val matchingInferences = entryContext.inferences
        .filter(i => searchWords.forall(i.name.toLowerCase.contains))
        .mapCollect { inference =>
          getSubstitutions(inference).map(InferenceSuggestion(inference.summary, inference.requiredSubstitutions, _))
        }
      val sortedInferences = if (withConclusion)
        matchingInferences.sortBy(_.inference.conclusion.complexity)(implicitly[Ordering[Int]].reverse)
      else
        matchingInferences.reverse
      sortedInferences.take(10)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String,
    @RequestParam("withConclusion") withConclusion: Boolean
  ): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      entryContext = EntryContext.forEntry(books, book, chapter, theorem)
      (step, stepContext, premiseContext) <- findStep[Step](theorem, stepPath, entryContext)
      inference <- findInference(inferenceId)(entryContext)
      targetOption <- if (withConclusion)
        step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target").map(_.statement).map(Some(_))
      else
        Success(None)
    } yield {
      getPremiseSuggestions(inference.premises, targetOption, inference, stepContext, premiseContext)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestNamingInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestNamingInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      entryContext = EntryContext.forEntry(books, book, chapter, theorem)
      (step, stepContext, _) <- findStep[Step.Target](theorem, stepPath, entryContext)
    } yield {
      ProofHelper.findNamingInferences(entryContext)
        .filter(_._1.name.toLowerCase.contains(searchText.toLowerCase))
        .reverse
        .mapCollect { case (inference, namingPremises, _) =>
          val substitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, stepContext.externalDepth)
          if (substitutions.nonEmpty)
            Some(InferenceSuggestion(inference.summary.copy(premises = namingPremises), inference.requiredSubstitutions, Some(substitutions)))
          else
            None
        }
        .take(10)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestNamingPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForNaming(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      entryContext = EntryContext.forEntry(books, book, chapter, theorem)
      (step, stepContext, premiseContext) <- findStep[Step.Target](theorem, stepPath, entryContext)
      inference <- findInference(inferenceId)(entryContext)
      (namingPremises, _) <- ProofHelper.getNamingPremisesAndAssumption(inference, entryContext).orBadRequest(s"Inference $inferenceId was not naming inference")
    } yield {
      getPremiseSuggestions(
        namingPremises,
        Some(step.statement),
        inference,
        stepContext,
        premiseContext)
    }).toResponseEntity
  }

  case class PremiseSuggestions(immediateSubstitutions: Option[Substitutions], premiseMatches: Seq[Seq[PossiblePremiseMatch]])
  case class PossiblePremiseMatch(statement: Statement, substitutions: Seq[Substitutions])
  private def getPremiseSuggestions(
    premises: Seq[Statement],
    targetOption: Option[Statement],
    inference: Inference,
    stepContext: StepContext,
    premiseContext: PremiseContext
  ): PremiseSuggestions = {
    val possibleConclusionSubstitutions = targetOption
      .map(inference.conclusion.calculateSubstitutions(_, Substitutions.empty, 0, stepContext.externalDepth))
      .getOrElse(Seq(Substitutions.empty))
    val availablePremises = (premiseContext.givenPremises ++ premiseContext.simplifiedPremises).map(_.statement).distinct
    val premiseMatches = premises.map { premise =>
      availablePremises.mapCollect { availablePremise =>
        val substitutions = for {
          conclusionSubstitutions <- possibleConclusionSubstitutions
          premiseSubstitutions <- premise.calculateSubstitutions(availablePremise, conclusionSubstitutions, 0, stepContext.externalDepth)
        } yield premiseSubstitutions
        if (substitutions.nonEmpty) {
          Some(PossiblePremiseMatch(availablePremise, substitutions))
        } else {
          None
        }
      }
    }
    val immediateSubstitutions = targetOption.flatMap(target =>
      premiseMatches.mapFind(_.mapFind(_.substitutions.find { substitutions =>
        Try(inference.substitutePremisesAndValidateConclusion(target, substitutions, stepContext)).toOption.exists(_.forall(availablePremises.contains))
      })))
    PremiseSuggestions(immediateSubstitutions, premiseMatches)
  }

}
