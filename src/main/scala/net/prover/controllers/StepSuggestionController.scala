package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model._
import net.prover.model.proof.{ProofHelper, Step, StepContext}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepSuggestionController @Autowired() (val bookService: BookService) extends BookModification {

  case class InferenceSuggestion(
    inference: Inference.Summary,
    requiredSubstitutions: Substitutions.Required,
    haveSubstitutionsOverflowed: Option[Boolean],
    substitutions: Option[Seq[Substitutions]])
  @GetMapping(value = Array("/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
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
      (step, stepContext) <- findStep[Step](theorem, proofIndex, stepPath, entryContext)
      getSubstitutions <- if (withConclusion)
        step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target").map { targetStep =>
          (inference: Inference) =>
            val substitutions = inference.conclusion.calculateSubstitutions(targetStep.statement, Substitutions.empty, 0, stepContext.externalDepth).take(11).toSeq
            if (substitutions.nonEmpty)
              if (substitutions.length > 10)
                Some(Some((true, Nil)))
              else
                Some(Some((false, substitutions)))
            else
              None
        }
        else
         Success((_: Inference) => Some(None))
    } yield {
      val matchingInferences = entryContext.inferences
        .filter(i => searchWords.forall(i.name.toLowerCase.contains))
        .mapCollect { inference =>
          getSubstitutions(inference).map { x =>
            InferenceSuggestion(inference.summary, inference.requiredSubstitutions, x.map(_._1), x.filter(!_._1).map(_._2))
          }
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
    @PathVariable("proofIndex") proofIndex: Int,
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
      (step, stepContext) <- findStep[Step](theorem, proofIndex, stepPath, entryContext)
      inference <- findInference(inferenceId)(entryContext)
      targetOption <- if (withConclusion)
        step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target").map(_.statement).map(Some(_))
      else
        Success(None)
    } yield {
      getPremiseSuggestions(inference.premises, targetOption, inference, stepContext)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestNamingInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestNamingInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      entryContext = EntryContext.forEntry(books, book, chapter, theorem)
      (step, stepContext) <- findStep[Step.Target](theorem, proofIndex, stepPath, entryContext)
    } yield {
      ProofHelper.findNamingInferences(entryContext)
        .filter(_._1.name.toLowerCase.contains(searchText.toLowerCase))
        .reverse
        .mapCollect { case (inference, namingPremises, _) =>
          val substitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, stepContext.externalDepth).take(11).toSeq
          if (substitutions.nonEmpty)
            Some(InferenceSuggestion(
              inference.summary.copy(premises = namingPremises),
              inference.requiredSubstitutions,
              Some(substitutions.length > 10),
              Some(substitutions).filter(_.length < 11)))
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
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      theorem <- findEntry[Theorem](chapter, theoremKey)
      entryContext = EntryContext.forEntry(books, book, chapter, theorem)
      (step, stepContext) <- findStep[Step.Target](theorem, proofIndex, stepPath, entryContext)
      inference <- findInference(inferenceId)(entryContext)
      (namingPremises, _) <- ProofHelper.getNamingPremisesAndAssumption(inference, entryContext).orBadRequest(s"Inference $inferenceId was not naming inference")
    } yield {
      getPremiseSuggestions(
        namingPremises,
        Some(step.statement),
        inference,
        stepContext)
    }).toResponseEntity
  }

  case class PremiseSuggestions(immediateSubstitutions: Option[Substitutions], premiseMatches: Seq[Seq[PossiblePremiseMatch]])
  case class PossiblePremiseMatch(statement: Statement, haveSubstitutionsOverflowed: Boolean, substitutions: Option[Seq[Substitutions]])
  private def getPremiseSuggestions(
    premises: Seq[Statement],
    targetOption: Option[Statement],
    inference: Inference,
    stepContext: StepContext
  ): PremiseSuggestions = {
    def possibleConclusionSubstitutions = targetOption
      .map(inference.conclusion.calculateSubstitutions(_, Substitutions.empty, 0, stepContext.externalDepth))
      .getOrElse(Iterator(Substitutions.empty))
    val availablePremises = stepContext.allPremisesSimplestLast.map(_.statement)
    val premiseMatches = premises.map { premise =>
      availablePremises.mapCollect { availablePremise =>
        val substitutions = (for {
          conclusionSubstitutions <- possibleConclusionSubstitutions
          premiseSubstitutions <- premise.calculateSubstitutions(availablePremise, conclusionSubstitutions, 0, stepContext.externalDepth)
        } yield premiseSubstitutions).take(11).toSeq
        if (substitutions.nonEmpty) {
          Some(PossiblePremiseMatch(availablePremise, substitutions.length > 10, if (substitutions.length < 11) Some(substitutions) else None))
        } else {
          None
        }
      }
    }
    val immediateSubstitutions = targetOption.flatMap(target =>
      premiseMatches.mapFind(_.mapFind(_.substitutions.toSeq.flatten.find { substitutions =>
        Try(inference.substitutePremisesAndValidateConclusion(target, substitutions, stepContext)).toOption.exists(_.forall(availablePremises.contains))
      })))
    PremiseSuggestions(immediateSubstitutions, premiseMatches)
  }

}
