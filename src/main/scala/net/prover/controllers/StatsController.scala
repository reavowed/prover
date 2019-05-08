package net.prover.controllers

import net.prover.model.EntryContext
import net.prover.model.entries.Theorem
import net.prover.model.expressions.DefinedStatement
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RequestParam, RestController}

@RestController
@RequestMapping(Array("/stats"))
class StatsController @Autowired() (val bookService: BookService) extends BookModification  {

  @GetMapping(value = Array("longestProofs"))
  def getLongestProofs: Seq[(String, Int)] = {
    val urlsWithLengths = for {
      (book, bookKey) <- getBooksWithKeys(bookService.books)
      (chapter, chapterKey) <- getChaptersWithKeys(book)
      (theorem, theoremKey) <- getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
    } yield (getEntryUrl(bookKey, chapterKey, theoremKey), theorem.proof.map(_.length).sum)
    urlsWithLengths
        .sortBy { case (_, length) => -1 * length }
        .take(10)
  }

  @GetMapping(value = Array("unusedInferences"))
  def getUnusedInferences: Seq[String] = {
    val entryContext = EntryContext.forBooks(bookService.books, Nil)
    val usedInferenceIds = entryContext.inferences.ofType[Theorem].flatMap(_.referencedInferenceIds)
    for {
      (book, bookKey) <- getBooksWithKeys(bookService.books)
      (chapter, chapterKey) <- getChaptersWithKeys(book)
      (inference, inferenceKey) <- getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
      if !usedInferenceIds.contains(inference.id)
    } yield getEntryUrl(bookKey, chapterKey, inferenceKey)
  }

  @GetMapping(value = Array("findAssertions"))
  def findAssertions(
    @RequestParam inferenceId: String,
    @RequestParam(required = false) statementSymbol: String
  ): Seq[(String, String)] = {
    val books = bookService.books
    for {
      (book, bookKey) <- getBooksWithKeys(books)
      (chapter, chapterKey) <- getChaptersWithKeys(book)
      (theorem, inferenceKey) <- getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
      (assertion, context) <- theorem.findAssertions(EntryContext.forEntry(books, book, chapter, theorem))
      if assertion.inference.id == inferenceId
      if Option(statementSymbol).forall(symbol =>
        assertion.statement.asOptionalInstanceOf[DefinedStatement]
          .exists(_.definition.symbol == symbol))
    } yield ("http://localhost:8080" + getEntryUrl(bookKey, chapterKey, inferenceKey), context.stepReference.stepPath.mkString("."))
  }
}
