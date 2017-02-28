package net.prover.model

case class BookLine(text: String, number: Int, bookTitle: String, fileName: String) {
  def asPartialLine: PartialLine = PartialLine(text, this)
}
