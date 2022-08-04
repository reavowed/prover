package net.prover.books.io

import net.prover.controllers.BookRepository
import net.prover.model.Book
import org.slf4j.{Logger, LoggerFactory}

object BookReader {
  val logger: Logger = LoggerFactory.getLogger(BookReader.getClass)
  def readBook(bookDefinition: BookDefinition, previousBooks: Seq[Book]): Book = {
    import bookDefinition._
    BookRepository.logger.info(s"Parsing book $title")
    Book.parse(title, BookDirectoryConfig.getBookPath(title), previousBooks, BookDirectoryConfig.getChapterPath(title, _, _))
  }
}
