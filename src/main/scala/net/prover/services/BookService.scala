package net.prover.services

import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorSystem, Props}
import net.prover.model._
import org.slf4j.LoggerFactory
import org.springframework.stereotype.Service

import scala.collection.JavaConverters._
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

@Service
class BookService {
  val bookDirectoryPath = Paths.get("books")
  val bookListPath = bookDirectoryPath.resolve("books.list")
  val cacheDirectoryPath = Paths.get("cache")

  val books = getBooks.getOrElse(Nil)

  def getBooks: Option[Seq[Book]] = {
    for {
      bookOutlines <- parseAllBooks()
      books <- proveBooks(bookOutlines)
    } yield books
  }

  private def getBookList: Seq[String] = {
    Files
      .readAllLines(bookListPath)
      .asScala
      .filter(s => !s.startsWith("#"))
  }

  private def getBookPath(title: String): Path = {
    val key = title.formatAsKey
    bookDirectoryPath.resolve(key).resolve(key + ".book")
  }

  private def getChapterPath(bookTitle: String, chapterTitle: String, chapterIndex: Int): Path = {
    bookDirectoryPath.resolve(bookTitle.formatAsKey).resolve("%02d".format(chapterIndex + 1) + "." + chapterTitle.camelCase + ".chapter")
  }

  private def parseBook(title: String, previousBookOutlines: Seq[BookOutline]): Option[BookOutline] = {
    BookOutline.parse(title, getBookPath(title), previousBookOutlines, getChapterPath(title, _, _))
      .ifEmpty { BookService.logger.info(s"Failed to parse book $title") }
  }

  private def parseAllBooks(): Option[Seq[BookOutline]] = {
    getBookList.mapFoldOption[BookOutline] { case (booksSoFar, bookTitle) =>
      parseBook(bookTitle, booksSoFar)
    }
  }

  def proveBooks(bookOutlines: Seq[BookOutline]): Option[Seq[Book]] = {
    val booksOption = bookOutlines.mapFoldOption[Book] { case (previousBooks, bookOutline) =>
        BookService.logger.info(s"Proving book ${bookOutline.title}")
        bookOutline.proveTheorems(cacheDirectoryPath, previousBooks)
    }
    booksOption.foreach { newBooks =>
      BookService.logger.info(s"Proved ${newBooks.length} books")
      newBooks.foreach { book =>
        Files.write(getBookPath(book.title), book.serialized.getBytes("UTF-8"))
        book.chapters.zipWithIndex.foreach { case (chapter, index) =>
          Files.write(getChapterPath(book.title, chapter.title, index), chapter.serialized.getBytes("UTF-8"))
        }
      }
    }
    booksOption
  }
}

object BookService {
  val logger = LoggerFactory.getLogger(BookService.getClass)
}
