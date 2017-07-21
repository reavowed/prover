package net.prover.services

import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorSystem, Props}
import net.prover.model._
import org.slf4j.LoggerFactory
import org.springframework.stereotype.Service

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try

@Service
class BookService {
  val bookDirectoryPath = Paths.get("books")
  val bookListPath = bookDirectoryPath.resolve("books.list")
  val cacheDirectoryPath = Paths.get("cache")
  val system = ActorSystem("BookService")
  import system.dispatcher

  val books = new AtomicReference[Seq[Book]](Nil)

  class BookManagementActor extends Actor {
    case class BookData(
      title: String,
      book: Option[Book],
      lastModificationTimesOfFiles: Map[Path, Instant])

    var allBookData: Seq[BookData] = Nil

    private def getBookList: Seq[String] = {
      Files
        .readAllLines(bookListPath)
        .asScala
        .filter(s => !s.startsWith("#"))
    }

    private def parseBook(title: String, currentBookData: Seq[BookData]): BookData = {
      val key = title.formatAsKey
      val path = bookDirectoryPath.resolve(key).resolve(key + ".book")
      val bookModificationTime = Files.getLastModifiedTime(path).toInstant
      val availableBooks = currentBookData.mapCollect(_.book)
      val parser = Book.parser(path, cacheDirectoryPath, availableBooks)
      Try(parser.parse(Tokenizer.fromPath(path))._1).toOption match {
        case Some((book, modificationTimes)) =>
          BookData(title, Some(book), modificationTimes.updated(path, bookModificationTime))
        case None =>
          BookData(title, None, Map(path -> bookModificationTime))
      }
    }

    private def isClean(bookData: BookData, dirtyBooks: Seq[String]) = {
      bookData.book.forall(_.dependencies.forall { dependency => !dirtyBooks.contains(dependency.title) }) &&
        bookData.lastModificationTimesOfFiles.forall { case (path, instant) =>
          Files.getLastModifiedTime(path).toInstant == instant
        }
    }

    private def updateBooks(): Unit = {
      val (changedBooks, newBookData) = getBookList
        .foldLeft((Seq.empty[String], Seq.empty[BookData])) { case ((dirtyBooks, bookDataSoFar), bookTitle) =>
          allBookData.find(_.title == bookTitle) match {
            case Some(bookData) if isClean(bookData, dirtyBooks) =>
              (dirtyBooks, bookDataSoFar :+ bookData)
            case Some(BookData(_, Some(book), _)) =>
              BookService.logger.info(s"Book '$bookTitle' has changed - rechecking")
              val bookData = parseBook(bookTitle, bookDataSoFar).ifEmpty(_.book) {
                BookService.logger.warn(s"Book '$bookTitle' is now invalid")
              }
              (dirtyBooks :+ bookTitle, bookDataSoFar :+ bookData)
            case Some(BookData(_, None, _)) =>
              val bookData = parseBook(bookTitle, bookDataSoFar).ifDefined(_.book) {
                BookService.logger.info(s"Book '$bookTitle' is now valid again")
              }
              (dirtyBooks :+ bookTitle, bookDataSoFar :+ bookData)
            case None =>
              BookService.logger.info(s"Book '$bookTitle' is new - rechecking")
              val bookData = parseBook(bookTitle, bookDataSoFar).ifEmpty(_.book) {
                BookService.logger.warn(s"Book '$bookTitle' was invalid")
              }
              (dirtyBooks :+ bookTitle, bookDataSoFar :+ bookData)
          }
        }
      if (changedBooks.nonEmpty) BookService.logger.info(s"Updated ${changedBooks.size} books")
      allBookData = newBookData
      newBookData.map(_.book).traverseOption.foreach(books.getAndSet)
    }

    override def receive: Receive = {
      case BookManagementActor.PollMessage =>
        updateBooks()
        system.scheduler.scheduleOnce(1.second, self, BookManagementActor.PollMessage)
    }
  }
  object BookManagementActor {
    object PollMessage
  }

  val serviceActor = system.actorOf(Props(new BookManagementActor))
  serviceActor ! BookManagementActor.PollMessage
}

object BookService {
  val logger = LoggerFactory.getLogger(BookService.getClass)
}
