package net.prover.controllers

import net.prover.model.EntryContext
import net.prover.model.entries.Theorem
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/stats"))
class StatsController @Autowired() (val bookService: BookService) extends BookModification  {

  @GetMapping(value = Array("longestProofs"))
  def getLongestProofs = {
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
  def getUnusedInferences = {
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
}
