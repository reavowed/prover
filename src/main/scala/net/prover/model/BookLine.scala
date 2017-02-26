package net.prover.model

case class BookLine(text: String, number: Int, bookTitle: String, fileName: String) {
  def splitFirstWord: (String, PartialLine) = {
    text.splitFirstWord.mapRight(PartialLine(_, this))
  }
  def asPartialLine: PartialLine = PartialLine(text, this)
}
