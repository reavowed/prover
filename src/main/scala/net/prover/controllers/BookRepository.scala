package net.prover.controllers

import java.nio.file.{Files, Path, Paths}

import net.prover.model._
import net.prover.model.definitions.Definitions
import org.apache.commons.io.FileUtils
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.stereotype.Service
import scalaz.Functor
import scalaz.syntax.functor._

import scala.collection.JavaConverters._
import scala.util.Try

@Service
class BookRepository {
  private val bookDirectoryPath = Paths.get("books")

  private var _booksAndDefinitions: (Seq[Book], Definitions) = this.synchronized {
    val books = parseBooks
    val definitions = getDefinitions(books)
    writeBooks(books)
    (books, definitions)
  }
  def booksAndDefinitions: (Seq[Book], Definitions) = this.synchronized {
    _booksAndDefinitions
  }
  def books: Seq[Book] = booksAndDefinitions._1

  def modifyBooks[F[_] : Functor](f: (Seq[Book], Definitions) => F[Seq[Book]]): F[(Seq[Book], Definitions)] = this.synchronized {
    val (books, definitions) = booksAndDefinitions
    for {
      newBooks <- f(books, definitions)
    } yield {
      val newDefinitions = getDefinitions(newBooks)
      writeBooks(newBooks)
      _booksAndDefinitions = (newBooks, newDefinitions)
      (newBooks, newDefinitions)
    }
  }

  def reload(): Try[Any] = {
    modifyBooks((_, _) => Try(parseBooks))
  }

  private def parseBooks: Seq[Book] = {
    val books = getBookList.mapReduceWithPrevious[Book] { case (booksSoFar, bookTitle) =>
      parseBook(bookTitle, booksSoFar)
    }
    BookRepository.logger.info(s"Parsed ${books.length} books")
    books
  }

  private def parseBook(title: String, previousBooks: Seq[Book]): Book = {
    BookRepository.logger.info(s"Parsing book $title")
    Book.parse(title, getBookPath(title), previousBooks, getChapterPath(title, _, _))
  }

  private def getBookList: Seq[String] = {
    Files
      .readAllLines(getBookListPath)
      .asScala
      .filter(s => !s.startsWith("#"))
  }

  private def writeBooks(books: Seq[Book]): Unit = {
    FileUtils.cleanDirectory(bookDirectoryPath.toFile)
    writeBooklist(books)
    books.foreach(writeBook)
  }

  private def writeBooklist(books: Seq[Book]): Unit = {
    Files.write(getBookListPath, (books.map(_.title).mkString("\n") + "\n").getBytes("UTF-8"))
  }

  private def writeBook(book: Book): Unit = {
    val bookPath = getBookPath(book.title)
    Files.createDirectories(bookPath.getParent)
    Files.write(bookPath, book.serialized.getBytes("UTF-8"))
    book.chapters.foreachWithIndex((chapter, index) =>
      Files.write(getChapterPath(book.title, chapter.title, index), chapter.serialized.getBytes("UTF-8"))
    )
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

  private def getDefinitions(books: Seq[Book]): Definitions = {
    Definitions(EntryContext(books.flatMap(_.chapters).flatMap(_.entries)))
  }
}

object BookRepository {
  val logger: Logger = LoggerFactory.getLogger(BookRepository.getClass)
}
