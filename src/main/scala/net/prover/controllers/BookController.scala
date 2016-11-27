package net.prover.controllers

import net.prover.model.Book
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/book"))
class BookController {

  @GetMapping(Array(""))
  def get: Book = {
    Book.fromFile("propositionalCalculus.book")
  }
}
