package net.prover.entries

import net.prover.books.keys.ListWithKeys
import net.prover.books.model.Book
import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.model.definitions.Definitions
import net.prover.model.entries.ChapterEntry

import scala.reflect.ClassTag
import scala.util.Try

case class GlobalContext(booksWithKeys: ListWithKeys[Book])
{
  lazy val definitions: Definitions = Definitions(booksWithContexts)
  val allBooks: List[Book] = booksWithKeys.list
  def booksWithContexts: List[BookWithContext] = booksWithKeys.listWithKeys.map { case (book, bookKey) => BookWithContext(book, bookKey, this) }

  def allEntries: List[EntryWithContext] = booksWithContexts.flatMap(_.chaptersWithContexts).flatMap(_.entriesWithContexts)
  def allTheorems: List[TheoremWithContext] = booksWithContexts.flatMap(_.chaptersWithContexts).flatMap(_.theoremsWithContexts)

  def findBook(bookKey: String): Try[BookWithContext] = {
    booksWithKeys.listWithKeys.find(_._2 == bookKey).map(_._1).map(BookWithContext(_, bookKey, this)).orNotFound(s"Book $bookKey")
  }
  def findChapter(bookKey: String, chapterKey: String): Try[ChapterWithContext] = {
    findBook(bookKey).flatMap(_.getChapter(chapterKey))
  }
  def findEntry[T <: ChapterEntry : ClassTag](bookKey: String, chapterKey: String, entryKey: String): Try[TypedEntryWithContext[T]] = {
    findChapter(bookKey, chapterKey).flatMap(_.getEntry[T](entryKey))
  }
}
