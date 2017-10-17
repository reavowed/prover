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
      .map(t => (s"/books/${t.bookKey}/${t.chapterKey}/${t.key}", t.proof.length))
  }
}
