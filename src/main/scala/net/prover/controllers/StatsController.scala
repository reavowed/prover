package net.prover.controllers

import net.prover.model.{EntryContext, Inference}
import net.prover.model.entries.Theorem
import net.prover.model.expressions.DefinedStatement
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RequestParam, RestController}

@RestController
@RequestMapping(Array("/stats"))
class StatsController @Autowired() (val bookService: BookService) extends BookModification  {

  @GetMapping(value = Array("longestProofs"))
  def getLongestProofs: Seq[(String, Int)] = {
    val urlsWithLengths = for {
      (book, bookKey) <- bookService.getBooksWithKeys
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      (theorem, theoremKey) <- BookService.getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
    } yield (BookService.getEntryUrl(bookKey, chapterKey, theoremKey), theorem.proofs.map(_.steps.map(_.length).sum).min)
    urlsWithLengths
        .sortBy { case (_, length) => -1 * length }
        .take(10)
  }

  @GetMapping(value = Array("unusedInferences"))
  def getUnusedInferences: Seq[String] = {
    val entryContext = EntryContext.forBooks(bookService.books, Nil)
    val usedInferenceIds = entryContext.inferences.ofType[Theorem].flatMap(_.referencedInferenceIds)
    for {
      (book, bookKey) <- bookService.getBooksWithKeys
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      (inference, inferenceKey) <- BookService.getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Inference]))
      if !usedInferenceIds.contains(inference.id)
    } yield BookService.getEntryUrl(bookKey, chapterKey, inferenceKey)
  }

  @GetMapping(value = Array("unprovenTheorems"))
  def getUnprovenTheorems: Seq[String] = {
    val (books, definitions) = bookService.booksAndDefinitions
    for {
      (book, bookKey) <- BookService.getBooksWithKeys(books)
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      (theorem, theoremKey) <- BookService.getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
      if !theorem.isComplete(definitions)
    } yield BookService.getEntryUrl(bookKey, chapterKey, theoremKey)
  }

  @GetMapping(value = Array("findAssertions"))
  def findAssertions(
    @RequestParam inferenceId: String,
    @RequestParam(required = false) statementSymbol: String
  ): Seq[(String, String)] = {
    for {
      (book, bookKey) <- bookService.getBooksWithKeys
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      (theorem, inferenceKey) <- BookService.getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
      (assertion, context) <- theorem.findSteps[Step.Assertion]
      if assertion.inference.id == inferenceId
      if Option(statementSymbol).forall(symbol =>
        assertion.statement.asOptionalInstanceOf[DefinedStatement]
          .exists(_.definition.symbol == symbol))
    } yield ("http://localhost:8080" + BookService.getEntryUrl(bookKey, chapterKey, inferenceKey), context.stepReference.stepPath.mkString("."))
  }

  @GetMapping(value = Array("findElisions"))
  def findElisions(
    @RequestParam inferenceId: String
  ): Seq[(String, String)] = {
    for {
      (book, bookKey) <- bookService.getBooksWithKeys
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      (theorem, inferenceKey) <- BookService.getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
      (elision, context) <- theorem.findSteps[Step.Elided]
      if elision.highlightedInference.exists(_.id == inferenceId)
    } yield ("http://localhost:8080" + BookService.getEntryUrl(bookKey, chapterKey, inferenceKey), context.stepReference.stepPath.mkString("."))
  }
}
