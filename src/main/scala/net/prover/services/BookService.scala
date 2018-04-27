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
  val system = ActorSystem("BookService")
  import system.dispatcher

  val books = new AtomicReference[Seq[Book]](Nil)

  class BookManagementActor extends Actor {
    case class BookData(
      title: String,
      bookOutline: Option[BookOutline],
      lastModificationTimesOfFiles: Map[Path, Instant])
    {
      def haveFilesChanged = {
        lastModificationTimesOfFiles.exists { case (path, instant) =>
          !Files.exists(path) || Files.getLastModifiedTime(path).toInstant != instant
        }
      }

      def haveDependenciesChanged(changedBookTitles: Seq[String]) = {
        bookOutline match {
          case Some(b) => b.dependencyOutlines.exists(d => changedBookTitles.contains(d.title))
          case None => changedBookTitles.nonEmpty
        }
      }
    }

    var allBookData: Seq[BookData] = Nil

    private def getBookList: Seq[String] = {
      Files
        .readAllLines(bookListPath)
        .asScala
        .filter(s => !s.startsWith("#"))
    }

    private def parseBook(title: String, previousBookData: Seq[BookData]): BookData = {
      val previousBookOutlines = previousBookData.mapCollect(_.bookOutline)
      val (bookOption, modificationTimes) = BookOutline.parse(title, bookDirectoryPath, previousBookOutlines)
      bookOption.ifEmpty { BookService.logger.info(s"Failed to parse book $title")}
      BookData(title, bookOption, modificationTimes)
    }

    private def parseAllBooks(): (Seq[String], Seq[BookData]) = {
      getBookList.foldLeft(
        (Seq.empty[String], Seq.empty[BookData])
      ) { case ((dirtyBooks, bookDataSoFar), bookTitle) =>
          allBookData.find(_.title == bookTitle) match {
            case Some(bookData) if bookData.haveFilesChanged =>
              BookService.logger.info(s"Book '$bookTitle' has changed")
              val newBookData = parseBook(bookTitle, bookDataSoFar)
              (dirtyBooks :+ bookTitle, bookDataSoFar :+ newBookData)
            case Some(bookData) if bookData.haveDependenciesChanged(dirtyBooks) =>
              (dirtyBooks :+ bookTitle, bookDataSoFar :+ bookData)
            case Some(bookData) =>
              (dirtyBooks, bookDataSoFar :+ bookData)
            case None =>
              BookService.logger.info(s"Book '$bookTitle' is new")
              val bookData = parseBook(bookTitle, bookDataSoFar)
              (dirtyBooks :+ bookTitle, bookDataSoFar :+ bookData)
          }
        }
    }

    def proveBooks(dirtyBooks: Seq[String], newBookData: Seq[BookData]): Unit = {
      val oldBooks = books.get()
      if (dirtyBooks.nonEmpty) {
        newBookData.map(_.bookOutline).traverseOption.foreach { bookOutlines =>
          val booksOption = bookOutlines.mapFoldOption[Book] { (previousBooks, bookOutline) =>
            oldBooks.find(_.title == bookOutline.title) match {
              case Some(book) if !dirtyBooks.contains(book.title) =>
                Some(book)
              case _ =>
                BookService.logger.info(s"Proving book ${bookOutline.title}")
                bookOutline.proveTheorems(cacheDirectoryPath, previousBooks)
            }
          }
          booksOption.foreach { newBooks =>
            books.set(newBooks)
          }
        }
        BookService.logger.info(s"Updated ${dirtyBooks.length} books")
      }
    }

    override def receive: Receive = {
      case BookManagementActor.PollMessage =>
        val (dirtyBooks, newBookData) = parseAllBooks()
        allBookData = newBookData
        proveBooks(dirtyBooks, newBookData)
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
