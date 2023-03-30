package net.prover.books.reading

import net.prover.books.keys.ListWithKeys
import net.prover.books.management.BookStateManager
import net.prover.books.model.{Book, BookTitle}
import net.prover.entries.{BookWithContext, GlobalContext}
import net.prover.model._
import net.prover.model.definitions.Definitions

object ReadBook {
  def apply(bookTitle: BookTitle, previousBooks: ListWithKeys[Book]): Book = {
    BookStateManager.logger.info(s"Parsing book $bookTitle")

    val outline = ReadBookOutline(bookTitle.value)
    val initialBook = Book(bookTitle.value, outline.imports, ListWithKeys.empty)
    outline.chapterTitles.zipWithIndex.foldLeft(initialBook) { case (book, (chapterTitle, index)) =>
      val chapter = ReadChapter(BookWithContext(book, previousBooks.keyAccumulator.getNextKey(book)._1, GlobalContext(previousBooks, Definitions(previousBooks.list))), chapterTitle, index)
      book.addChapter(chapter)
    }
  }
}
