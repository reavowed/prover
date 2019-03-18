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
      .map(theorem => (theorem.key.url, theorem.proof.length))
  }

  @GetMapping(value = Array("unusedInferences"))
  def getUnusedInferences = {
    val usedInferenceIds = bookService.books.flatMap(_.theorems).flatMap(_.referencedInferenceIds).toSet
    for {
      inference <- bookService.books.flatMap(_.inferences)
      if !usedInferenceIds.contains(inference.id)
    } yield inference.entryKey.url
  }
}
