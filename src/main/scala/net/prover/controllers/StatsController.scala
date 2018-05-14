package net.prover.controllers

import net.prover.services.BookService
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/stats"))
class StatsController @Autowired() (bookService: BookService) {

  @GetMapping(value = Array("longestProofs"))
  def getLongestProofs = {
    bookService.books.flatMap(_.theorems)
      .sortBy(theorem => -1 * theorem.proof.length)
      .take(10)
      .map(theorem => (s"/books/${theorem.bookKey}/${theorem.chapterKey}/${theorem.key}", theorem.proof.length))
  }

  @GetMapping(value = Array("unusedLines"))
  def getUnusedLines = {
    for {
      theorem <- bookService.books.flatMap(_.theorems)
      reference <- theorem.proof.steps.intermediateReferences
      if theorem.proof.referenceMap.getReferrers(reference).isEmpty
    } yield (s"/books/${theorem.bookKey}/${theorem.chapterKey}/${theorem.key}", reference)
  }
}
