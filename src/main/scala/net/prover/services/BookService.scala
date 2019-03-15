package net.prover.services

import java.nio.file.{Files, Path, Paths}

import net.prover.model._
import net.prover.model.entries._
import org.slf4j.LoggerFactory
import org.springframework.stereotype.Service

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

@Service
class BookService {
  val bookDirectoryPath = Paths.get("books")
  val cacheDirectoryPath = Paths.get("cache")

  private var _books = parseBooks.getOrElse(Nil)

  def books = _books

  def updateBooks(f: Seq[Book] => Seq[Book]) = updateBooksWithResult[Unit](books => (f(books), ()))

  def updateBooksWithResult[T](f: Seq[Book] => (Seq[Book], T)): T = synchronized {
    val (newBooks, result) = f(_books)
    writeBooks(newBooks)
    _books = newBooks
    result
  }

  def modifyChapter[T](bookKey: String, chapterKey: String)(f: (Seq[Book], Book, Chapter) => (Option[Chapter], T)): Option[T] = {
    updateBooksWithResult[Option[T]] { books =>
      (for {
        (book, bookIndex) <- books.findWithIndex(_.key.value == bookKey)
        (chapter, chapterIndex) <- book.chapters.findWithIndex(_.key.value == chapterKey)
      } yield {
        f(books, book, chapter).mapLeft {
          case Some(newChapter) =>
            books.updated(bookIndex, book.copy(chapters = book.chapters.updated(chapterIndex, newChapter)))
          case None =>
            books
        }
      }) match {
        case Some((newBooks, result)) => (newBooks, Some(result))
        case None => (books, None)
      }
    }
  }

  def addChapterEntry[T](bookKey: String, chapterKey: String)(f: (Seq[Book], Book, Chapter) => (Option[ChapterEntry], T)): Option[T] = {
    modifyChapter(bookKey, chapterKey) { (books, book, chapter) =>
      f(books, book, chapter).mapLeft(_.map(chapter.addEntry))
    }
  }

  def modifyEntry[T <: ChapterEntry.WithKey : ClassTag](bookKey: String, chapterKey: String, entryKey: String)(f: T => T): Option[Unit] = {
    modifyChapter(bookKey, chapterKey){ (_, _, chapter) =>
      (Some(chapter.copy(entries = chapter.entries.map(e => e.asOptionalInstanceOf[T].filter(_.key.value == entryKey).map(f).getOrElse(e)))), ())
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
