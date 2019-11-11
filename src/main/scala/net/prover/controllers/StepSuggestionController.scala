package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model._
import net.prover.model.entries.Theorem
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{ProofHelper, Step, StepContext}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}/{stepPath}"))
class StepSuggestionController @Autowired() (val bookService: BookService) extends BookModification {

  case class InferenceSuggestion(
    inference: Inference.Summary,
    requiredSubstitutions: Substitutions.Required,
    substitutions: Seq[Substitutions.Possible],
    rewriteInference: Option[Inference.Summary])

  @GetMapping(value = Array("/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
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
      (step, stepContext) <- findStep[Step](theorem, proofIndex, stepPath, entryContext)
      targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target")
    } yield {
      def getSuggestions(inference: Inference): Seq[InferenceSuggestion] = {
        val direct = inference.conclusion.calculateSubstitutions(targetStep.statement, stepContext).map { substitutions =>
          InferenceSuggestion(inference.summary, inference.requiredSubstitutions, Seq(substitutions), None)
        }
        def rewritten = for {
          (rewriteInference, rewritePremise) <- entryContext.rewriteInferences
          rewriteSubstitutions <- rewriteInference.conclusion.calculateSubstitutions(targetStep.statement, stepContext).flatMap(_.confirmTotality).toSeq
          rewrittenTarget <- rewritePremise.applySubstitutions(rewriteSubstitutions, stepContext).toSeq
          substitutions <- inference.conclusion.calculateSubstitutions(rewrittenTarget, stepContext).toSeq
        } yield InferenceSuggestion(inference.summary, inference.requiredSubstitutions, Seq(substitutions), Some(rewriteInference.summary))
        direct.map(Seq(_)).getOrElse(rewritten)
      }
      filterInferences(entryContext.inferences, searchText)
        .sortBy(_.conclusion.complexity)(implicitly[Ordering[Int]].reverse)
        .iterator
        .flatMap(getSuggestions)
        .take(10)
        .toSeq
    }).toResponseEntity
  }

  @GetMapping(value = Array("/suggestInferencesForPremise"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForPremise(
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
    } yield {
      filterInferences(entryContext.inferences, searchText)
        .reverse
        .take(10)
        .map { inference => InferenceSuggestion(inference.summary, inference.requiredSubstitutions, Seq(Substitutions.empty), None) }
    }).toResponseEntity
  }

  private def calculateSubstitutionsForTermOrSubTerm(targetTerm: Term, conclusionTerm: Term, stepContext: StepContext): Option[Seq[Substitutions.Possible]] = {
    conclusionTerm.calculateSubstitutions(targetTerm, stepContext).map(Seq(_)) orElse
      Some((targetTerm.getTerms(stepContext).map(_._1).toSet - targetTerm).toSeq.mapCollect(conclusionTerm.calculateSubstitutions(_, stepContext))).filter(_.nonEmpty)
  }

  @GetMapping(value = Array("/suggestInferencesForTransitivityFromLeft"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferencesForTransitivityFromLeft(
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
      (step, stepContext) <- findStep[Step](theorem, proofIndex, stepPath, entryContext)
      targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target")
      (targetLhs, transitivityDefinition) <- entryContext.getTransitivityDefinitions.mapFind { definition =>
        for {
          (lhs, _) <- definition.splitStatement(targetStep.statement)(stepContext)
        } yield (lhs, definition)
      }.orBadRequest("Target step is not a transitive statement")
    } yield {
      def getSuggestions(inference: Inference): Option[InferenceSuggestion] = {
        for {
          (conclusionLhs, _) <- transitivityDefinition.splitStatement(inference.conclusion)(stepContext)
          substitutions <- calculateSubstitutionsForTermOrSubTerm(targetLhs, conclusionLhs, stepContext)
        } yield InferenceSuggestion(
          inference.summary,
          inference.requiredSubstitutions,
          substitutions,
          None)
      }

      filterInferences(entryContext.inferences, searchText)
        .sortBy(_.conclusion.complexity)(implicitly[Ordering[Int]].reverse)
        .iterator
        .mapCollect(getSuggestions)
        .take(10)
        .toSeq
    }).toResponseEntity
  }

  private def filterInferences(inferences: Seq[Inference], searchText: String): Seq[Inference] = {
    val searchWords = searchText.toLowerCase().splitByWhitespace()
    inferences.filter(i => searchWords.forall(i.name.toLowerCase.contains))
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

  @GetMapping(value = Array("/suggestPremisesForTransitivityFromLeft"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForTransitivityFromLeft(
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
      targetStep <- step.asOptionalInstanceOf[Step.Target].orBadRequest("Step is not target")
      (targetLhs, transitivityDefinition) <- entryContext.getTransitivityDefinitions.mapFind { definition =>
        for {
          (lhs, _) <- definition.splitStatement(targetStep.statement)(stepContext)
        } yield (lhs, definition)
      }.orBadRequest("Target step is not a transitive statement")
      inference <- findInference(inferenceId)(entryContext)
      (conclusionLhs, _) <- transitivityDefinition.splitStatement(inference.conclusion)(stepContext).orBadRequest("Inference conclusion is not transitive statement")
      possibleSubstitutions <- calculateSubstitutionsForTermOrSubTerm(targetLhs, conclusionLhs, stepContext)
        .orBadRequest("Could not calculate any substitutions for this inference")
    } yield {
      val premiseMatches = getPremiseMatches(inference.premises, possibleSubstitutions, stepContext)
      PremiseSuggestions(None, premiseMatches)
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
          inference.conclusion.calculateSubstitutions(step.statement, stepContext)
            .map(s => InferenceSuggestion(
              inference.summary.copy(premises = namingPremises),
              inference.requiredSubstitutions,
              Seq(s),
              None))
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
  case class PossiblePremiseMatch(statement: Statement, substitutions: Seq[Substitutions.Possible])
  private def getPremiseSuggestions(
    premises: Seq[Statement],
    targetOption: Option[Statement],
    inference: Inference,
    stepContext: StepContext
  ): PremiseSuggestions = {
    def possibleConclusionSubstitutions = targetOption
      .map(inference.conclusion.calculateSubstitutions(_, stepContext).toSeq)
      .getOrElse(Seq(Substitutions.Possible.empty))
    val availablePremises = stepContext.allPremisesSimplestLast.map(_.statement)
    val premiseMatches = getPremiseMatches(premises, possibleConclusionSubstitutions, stepContext)
    val immediateSubstitutions = targetOption.flatMap(target =>
      premiseMatches.mapFind(_.mapFind(_.substitutions.mapFind { possibleSubstitutions =>
        possibleSubstitutions.confirmTotality.find { substitutions =>
          inference.substitutePremisesAndValidateConclusion(target, substitutions, stepContext).exists(_.forall(availablePremises.contains))
        }
      })))
    PremiseSuggestions(immediateSubstitutions, premiseMatches)
  }
  private def getPremiseMatches(
    premises: Seq[Statement],
    possibleConclusionSubstitutions: Seq[Substitutions.Possible],
    stepContext: StepContext
  ) = {
    val availablePremises = stepContext.allPremisesSimplestLast.map(_.statement)
    premises.map { premise =>
      availablePremises.mapCollect { availablePremise =>
        val substitutions = possibleConclusionSubstitutions.mapCollect(premise.calculateSubstitutions(availablePremise, _, stepContext))
        if (substitutions.nonEmpty) {
          Some(PossiblePremiseMatch(availablePremise, substitutions))
        } else {
          None
        }
      }
    }
  }
}
