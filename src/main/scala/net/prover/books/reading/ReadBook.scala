package net.prover.books.reading

import net.prover.books.keys.ListWithKeys
import net.prover.books.management.BookStateManager
import net.prover.books.model.{Book, BookTitle}
import net.prover.model._

object ReadBook {
  def apply(bookTitle: BookTitle, previousBooks: Seq[Book]): Book = {
    BookStateManager.logger.info(s"Parsing book $bookTitle")

    val outline = ReadBookOutline(bookTitle.value)
    val dependencies = Book.getDependencies(outline.imports, previousBooks)
    val entryContext = EntryContext.forBooks(dependencies)
    val chapters = outline.chapterTitles.zipWithIndex.mapFold(entryContext) { case (context, (chapterTitle, index)) =>
      ReadChapter(outline, chapterTitle, index, context).swap
    }._2
    Book(outline.title, outline.imports, ListWithKeys(chapters))
  }
}
