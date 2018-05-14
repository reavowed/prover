package net.prover.services

import java.nio.file.{Files, Path, Paths}

import net.prover.model._
import org.slf4j.LoggerFactory
import org.springframework.stereotype.Service

import scala.collection.JavaConverters._

@Service
class BookService {
  val bookDirectoryPath = Paths.get("books")
  val cacheDirectoryPath = Paths.get("cache")

  val books = parseBooks.getOrElse(Nil)

  def parseBooks: Option[Seq[Book]] = {
    val books = getBookList.mapFoldOption[Book] { case (booksSoFar, bookTitle) =>
      parseBook(bookTitle, booksSoFar)
    }
    books.foreach { books =>
      BookService.logger.info(s"Parsed ${books.length} books")
    }
    books
  }

  private def parseBook(title: String, previousBooks: Seq[Book]): Option[Book] = {
    BookService.logger.info(s"Parsing book $title")
    Book.parse(title, getBookPath(title), previousBooks, getChapterPath(title, _, _))
      .ifEmpty { BookService.logger.info(s"Failed to parse book $title") }
  }

  private def getBookList: Seq[String] = {
    Files
      .readAllLines(getBookListPath)
      .asScala
      .filter(s => !s.startsWith("#"))
  }

  private def getBookListPath: Path = {
    bookDirectoryPath.resolve("books.list")
  }

  private def getBookPath(title: String): Path = {
    val key = title.formatAsKey
    bookDirectoryPath.resolve(key).resolve(key + ".book")
  }

  private def getChapterPath(bookTitle: String, chapterTitle: String, chapterIndex: Int): Path = {
    bookDirectoryPath.resolve(bookTitle.formatAsKey).resolve("%02d".format(chapterIndex + 1) + "." + chapterTitle.camelCase + ".chapter")
  }
}

object BookService {
  val logger = LoggerFactory.getLogger(BookService.getClass)
}
