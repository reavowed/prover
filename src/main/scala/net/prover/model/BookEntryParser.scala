package net.prover.model

trait BookEntryParser {
  def name: String
  def parser(book: Book): Parser[Book]
}
