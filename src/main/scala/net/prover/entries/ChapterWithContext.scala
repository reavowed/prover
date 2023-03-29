package net.prover.entries

import net.prover.books.model.Book
import net.prover.controllers.{BookService, OptionWithResponseExceptionOps}
import net.prover.model.Chapter
import net.prover.model.definitions.Definitions
import net.prover.model.entries.ChapterEntry

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

case class ChapterWithContext(
    chapter: Chapter,
    chapterKey: String,
    bookWithContext: BookWithContext
) {
  def allBooks: List[Book] = bookWithContext.allBooks
  def definitions: Definitions = bookWithContext.definitions
  def globalContext: GlobalContext = bookWithContext.globalContext
  def book: Book = bookWithContext.book
  def bookKey: String = bookWithContext.bookKey

  def entriesWithKeys: List[(ChapterEntry, String)] = BookService.getEntriesWithKeys(chapter)

  def getEntry[T <: ChapterEntry : ClassTag](entryKey: String): Try[TypedEntryWithContext[T]] = {
    entriesWithKeys.find(_._2 == entryKey)
      .orNotFound(s"Chapter $chapterKey")
      .flatMap(_._1.asOptionalInstanceOf[T].orBadRequest(s"Entry is not a ${classTag[T].runtimeClass.getSimpleName}"))
      .map(TypedEntryWithContext(_, entryKey, this))
  }

  def getEntry[T <: ChapterEntry : ClassTag](entry: T): TypedEntryWithContext[T] = {
    val key = entriesWithKeys.find(_._1 == entry).map(_._2).get
    TypedEntryWithContext(entry, key, this)
  }
}
