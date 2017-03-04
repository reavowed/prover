package net.prover.model

object BookInclude extends BookEntryParser {
  override def name: String = "include"

  override def parser(book: Book): Parser[Book] = Parser { tokenizer =>
    val (pathText, nextTokenizer) = Parser.toEndOfLine.parse(tokenizer)
    val includeTokenizer = Tokenizer.fromPath(book.path.getParent.resolve(pathText)).copy(currentBook = Some(book.title))
    (book, nextTokenizer.addTokenizer(includeTokenizer))
  }
}
