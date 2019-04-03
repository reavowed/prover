package net.prover.controllers

import java.nio.file.{Files, Path, Paths}

import net.prover.model._
import net.prover.model.entries._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.stereotype.Service

import scala.collection.JavaConverters._
import scala.reflect._
import scala.util.Try

@Service
class BookService {
  private val bookDirectoryPath = Paths.get("books")

  private var _books = parseBooks

  def books: Seq[Book] = _books

  def modifyBooks[T](f: Seq[Book] => Try[(Seq[Book], T)]): Try[(Seq[Book], T)] = synchronized {
    for {
      (newBooks, result) <- f(_books)
    } yield {
      writeBooks(newBooks)
      _books = newBooks
      (newBooks, result)
    }
  }

  def reload(): Try[Any] = {
    modifyBooks[Unit](_ => Try(parseBooks).map(_ -> ()))
  }

  private def parseBooks: Seq[Book] = {
    val books = getBookList.mapReduceWithPrevious[Book] { case (booksSoFar, bookTitle) =>
      parseBook(bookTitle, booksSoFar)
    }
    BookService.logger.info(s"Parsed ${books.length} books")
    books
  }

  private def parseBook(title: String, previousBooks: Seq[Book]): Book = {
    BookService.logger.info(s"Parsing book $title")
    Book.parse(title, getBookPath(title), previousBooks, getChapterPath(title, _, _))
  }

  private def getBookList: Seq[String] = {
    Files
      .readAllLines(getBookListPath)
      .asScala
      .filter(s => !s.startsWith("#"))
  }

  private def writeBooks(books: Seq[Book]): Unit = {
    writeBooklist(books)
    books.foreach(writeBook)
  }

  private def writeBooklist(books: Seq[Book]): Unit = {
    Files.write(getBookListPath, (books.map(_.title).mkString("\n") + "\n").getBytes("UTF-8"))
  }

  private def writeBook(book: Book): Unit = {
    Files.write(getBookPath(book.title), book.serialized.getBytes("UTF-8"))
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
}

object BookService {
  val logger: Logger = LoggerFactory.getLogger(BookService.getClass)
}
