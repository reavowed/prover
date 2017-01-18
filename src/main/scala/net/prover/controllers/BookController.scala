package net.prover.controllers

import net.prover.model.Book
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/books"))
class BookController {

  def books = Book.fromDirectory("books")

  @GetMapping(Array(""))
  def get = {
    books
  }
}
