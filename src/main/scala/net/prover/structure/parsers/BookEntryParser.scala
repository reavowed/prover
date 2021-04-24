package net.prover.structure.parsers

import net.prover.model.Parser
import net.prover.structure.model.Book

trait BookEntryParser {
  def name: String
  def parser(book: Book): Parser[Book]
}