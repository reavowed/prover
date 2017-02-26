package net.prover.model

trait BookEntryParser {
  def name: String
  def parser(book: Book, lines: Seq[BookLine]): Parser[(Book, Seq[BookLine])]
}
