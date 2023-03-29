package net.prover.entries

import net.prover.books.keys.GetWithKeys
import net.prover.books.model.Book
import net.prover.controllers.{BookService, OptionWithResponseExceptionOps}
import net.prover.model.definitions.Definitions
import net.prover.model.entries.ChapterEntry

import scala.reflect.ClassTag
import scala.util.Try

case class GlobalContext(
    allBooks: List[Book],
    definitions: Definitions
) {
  val booksWithKeys: List[(Book, String)] = GetWithKeys(allBooks)
  def booksWithContexts: List[BookWithContext] = booksWithKeys.map { case (book, bookKey) => BookWithContext(book, bookKey, this) }

  def findBook(bookKey: String): Try[BookWithContext] = {
    booksWithKeys.find(_._2 == bookKey).map(_._1).map(BookWithContext(_, bookKey, this)).orNotFound(s"Book $bookKey")
  }
  def findChapter(bookKey: String, chapterKey: String): Try[ChapterWithContext] = {
    findBook(bookKey).flatMap(_.getChapter(chapterKey))
  }
  def findEntry[T <: ChapterEntry : ClassTag](bookKey: String, chapterKey: String, entryKey: String): Try[TypedEntryWithContext[T]] = {
    findChapter(bookKey, chapterKey).flatMap(_.getEntry[T](entryKey))
  }
}
