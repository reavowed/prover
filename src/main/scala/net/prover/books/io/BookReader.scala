package net.prover.books.io

import net.prover.controllers.BookRepository
import net.prover.model._
import org.slf4j.{Logger, LoggerFactory}

object BookReader {
  val logger: Logger = LoggerFactory.getLogger(BookReader.getClass)

  def readBook(bookDefinition: BookDefinition, previousBooks: Seq[Book]): Book = {
    import bookDefinition._
    BookRepository.logger.info(s"Parsing book $title")

    val outline = parseOutline(title)
    val dependencies = Book.getDependencies(outline.imports, previousBooks)
    val entryContext = EntryContext.forBooks(dependencies)
    val chapters = outline.chapterTitles.zipWithIndex.mapFold(entryContext) { case (context, (chapterTitle, index)) =>
      ChapterReader.readChapter(outline, chapterTitle, index, context).swap
    }._2
    Book(outline.title, outline.imports, chapters)
  }

  private def parseOutline(title: String): BookOutline = {
    val path = BookDirectoryConfig.getBookFilePath(title)
    val parser = for {
      imports <- Parser.optional("import", Parser.toEndOfLine).whileDefined
      chapterTitles <- Parser.optional("chapter", Parser.toEndOfLine).whileDefined
    } yield BookOutline(title, imports, chapterTitles)

    parser.parseFromFile(path, s"book '$title'")
  }
}
