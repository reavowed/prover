package net.prover.controllers

import net.prover.entries.GlobalContext

import javax.servlet.http.HttpServletRequest
import net.prover.model.{EntryContext, Inference}
import net.prover.model.entries.Theorem
import net.prover.model.expressions.DefinedStatement
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RequestParam, RestController}

import java.util.regex.Pattern

@RestController
@RequestMapping(Array("/stats"))
class StatsController @Autowired() (val bookService: BookService) {

  @GetMapping(value = Array("longestProofs"))
  def getLongestProofs(
    request: HttpServletRequest
  ): Seq[(String, Int)] = {
    val urlsWithLengths = for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      theoremWithContext <- chapterWithContext.theoremsWithContexts
    } yield ("http://" + request.getHeader("Host") + BookService.getEntryUrl(theoremWithContext), theoremWithContext.theorem.proofs.map(_.steps.map(_.length).sum).min)
    urlsWithLengths
        .sortBy { case (_, length) => -1 * length }
        .take(10)
  }

  @GetMapping(value = Array("unusedInferences"))
  def getUnusedInferences(
    request: HttpServletRequest
  ): Seq[String] = {
    val entryContext = EntryContext.forBooks(bookService.globalContext.booksWithContexts)
    val usedInferenceIds = entryContext.allInferences.ofType[Theorem].flatMap(_.referencedInferenceIds)
    for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      inferenceWithContext <- chapterWithContext.inferencesWithContexts
      if !usedInferenceIds.contains(inferenceWithContext.entry.id)
    } yield "http://" + request.getHeader("Host") + BookService.getEntryUrl(inferenceWithContext)
  }

  @GetMapping(value = Array("unprovenTheorems"))
  def getUnprovenTheorems(request: HttpServletRequest): Seq[String] = {
    val globalContext = bookService.globalContext
    for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      theoremWithContext <- chapterWithContext.theoremsWithContexts
      if !theoremWithContext.theorem.isComplete(globalContext.definitions)
    } yield "http://" + request.getHeader("Host") + BookService.getEntryUrl(theoremWithContext)
  }

  @GetMapping(value = Array("findAssertions"))
  def findAssertions(
    request: HttpServletRequest,
    @RequestParam inferenceId: String,
    @RequestParam(required = false) statementSymbol: String
  ): Seq[(String, String)] = {
    for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      theoremWithContext <- chapterWithContext.theoremsWithContexts
      (assertion, context) <- theoremWithContext.theorem.findSteps[Step.Assertion]
      if assertion.inference.id == inferenceId
      if Option(statementSymbol).forall(symbol =>
        assertion.statement.asOptionalInstanceOf[DefinedStatement]
          .exists(_.definition.symbol == symbol))
    } yield ("http://" + request.getHeader("Host") + BookService.getEntryUrl(theoremWithContext), context.stepReference.stepPath.mkString("."))
  }

  @GetMapping(value = Array("findElisions"))
  def findElisions(
    request: HttpServletRequest,
    @RequestParam inferenceId: String
  ): Seq[(String, String)] = {
    for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      theoremWithContext <- chapterWithContext.theoremsWithContexts
      (elision, context) <- theoremWithContext.theorem.findSteps[Step.Elided]
      if elision.highlightedInference.exists(_.id == inferenceId)
    } yield ("http://" + request.getHeader("Host") + BookService.getEntryUrl(theoremWithContext), context.stepReference.stepPath.mkString("."))
  }

  @GetMapping(value = Array("nonAlphanumericTheorems"))
  def findNonAlphanumericTheorems(
    request: HttpServletRequest
  ): Seq[String] = {
    for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      theoremWithContext <- chapterWithContext.theoremsWithContexts
      if Pattern.compile("[^A-Za-z0-9-'()]").matcher(theoremWithContext.entryKey).find()
    } yield "http://" + request.getHeader("Host") + BookService.getEntryUrl(theoremWithContext)
  }
}
