package net.prover.model

trait BookEntryParser {
  def name: String
  def parse(remainingLine: PartialLine, otherLines: Seq[BookLine], book: Book): (Book, Seq[BookLine])
}
