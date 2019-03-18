package net.prover.services

import java.nio.file.{Files, Path, Paths}

import net.prover.exceptions.NotFoundException
import net.prover.model._
import net.prover.model.entries._
import org.slf4j.LoggerFactory
import org.springframework.stereotype.Service

import scala.collection.JavaConverters._
import scala.reflect._
import scala.util.{Failure, Success, Try}

@Service
class BookService {
  private val bookDirectoryPath = Paths.get("books")
  private val cacheDirectoryPath = Paths.get("cache")

  private var _books = parseBooks.getOrElse(Nil)

  def books: Seq[Book] = _books

  def modifyBooks[T](f: Seq[Book] => Try[(Seq[Book], T)]): Try[T] = synchronized {
    val newBooksAndResult = f(_books)
    newBooksAndResult.map(_._1).foreach { newBooks =>
      writeBooks(newBooks)
      _books = newBooks
    }
    newBooksAndResult.map(_._2)
  }

  def modifyBook[T](bookKey: String)(f: (Seq[Book], Book) => Try[(Book, T)]): Try[T] = {
    modifyBooks[T] { books =>
      books.updateSingleIfDefinedWithResult {
        case book if book.key.value == bookKey =>
          f(books, book)
      }.orException(NotFoundException(s"Book $bookKey"))
    }
  }

  def modifyChapter[T](bookKey: String, chapterKey: String)(f: (Seq[Book], Book, Chapter) => Try[(Chapter, T)]): Try[T] = {
    modifyBook[T](bookKey) { (books, book) =>
      book.chapters
        .updateSingleIfDefinedWithResult {
          case chapter if chapter.key.value == chapterKey =>
            f(books, book, chapter)
        }
        .orException(NotFoundException(s"Chapter $chapterKey"))
        .map(_.mapLeft(newChapters => book.copy(chapters = newChapters)))
    }
  }

  def modifyEntry[TEntry <: ChapterEntry.WithKey : ClassTag, TResult](bookKey: String, chapterKey: String, entryKey: String)(f: (Seq[Book], Book, Chapter, TEntry) => Try[(TEntry, TResult)]): Try[TResult] = {
    modifyChapter[TResult](bookKey, chapterKey) { (books, book, chapter) =>
      chapter.entries
        .updateSingleIfDefinedWithResult {
          case entry if entry.isRuntimeInstance[TEntry] && entry.asInstanceOf[TEntry].key.value == entryKey =>
            f(books, book, chapter, entry.asInstanceOf[TEntry])
        }
        .orException(NotFoundException(s"${classTag[TEntry].runtimeClass.getSimpleName} $entryKey"))
        .map(_.mapLeft(newEntries => chapter.copy(entries = newEntries)))
    }
  }

  def addChapterEntry[T](bookKey: String, chapterKey: String)(f: (Seq[Book], Book, Chapter) => Try[(ChapterEntry, T)]): Try[T] = {
    modifyChapter(bookKey, chapterKey) { (books, book, chapter) =>
      f(books, book, chapter).map(_.mapLeft(chapter.addEntry))
    }
  }

  private def parseBooks: Option[Seq[Book]] = {
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

  private def writeBooks(books: Seq[Book]) = {
    writeBooklist(books)
    books.foreach(writeBook)
  }

  private def writeBooklist(books: Seq[Book]) = {
    Files.write(getBookListPath, (books.map(_.title).mkString("\n") + "\n").getBytes("UTF-8"))
  }

  private def writeBook(book: Book) = {
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
  val logger = LoggerFactory.getLogger(BookService.getClass)
}
