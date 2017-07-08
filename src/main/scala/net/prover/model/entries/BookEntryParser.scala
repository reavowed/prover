package net.prover.model.entries

import net.prover.model.{Book, Parser}

trait BookEntryParser {
  def name: String
  def parser(book: Book): Parser[Book]
}
