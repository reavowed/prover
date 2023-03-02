package net.prover.model.entries

import net.prover.books.model.Book
import net.prover.model.Parser

trait BookEntryParser {
  def name: String
  def parser(book: Book): Parser[Book]
}
