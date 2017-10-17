package net.prover.controllers

import net.prover.model.Book
import net.prover.services.BookService
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/stats"))
class StatsController @Autowired() (bookService: BookService) {

  def getBooks: Seq[Book] = bookService.books.get()

  @GetMapping(value = Array("longestProofs"))
  def getLongestProofs = {
    getBooks.flatMap(_.theorems)
      .sortBy(theorem => -1 * theorem.proof.length)
      .take(10)
      .map(theorem => (s"/books/${theorem.bookKey}/${theorem.chapterKey}/${theorem.key}", theorem.proof.length))
  }

  @GetMapping(value = Array("unusedLines"))
  def getUnusedLines = {
    for {
      theorem <- getBooks.flatMap(_.theorems)
      reference <- theorem.proof.steps.flatMap(_.intermediateReferences)
      if theorem.proof.referenceMap.getReferrers(reference).isEmpty
      if !theorem.proof.steps.lastOption.exists(_.reference.value == reference)
    } yield (s"/books/${theorem.bookKey}/${theorem.chapterKey}/${theorem.key}", reference)
  }
}
