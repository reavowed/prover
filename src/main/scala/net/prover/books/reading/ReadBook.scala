package net.prover.books.reading

import net.prover.books.management.BookStateManager
import net.prover.books.model.{Book, BookDefinition}
import net.prover.model._

object ReadBook {
  def apply(bookDefinition: BookDefinition, previousBooks: Seq[Book]): Book = {
      import bookDefinition._
      BookStateManager.logger.info(s"Parsing book $title")

      val outline = ReadBookOutline(title)
      val dependencies = Book.getDependencies(outline.imports, previousBooks)
      val entryContext = EntryContext.forBooks(dependencies)
      val chapters = outline.chapterTitles.zipWithIndex.mapFold(entryContext) { case (context, (chapterTitle, index)) =>
        ReadChapter(outline, chapterTitle, index, context).swap
      }._2
      Book(outline.title, outline.imports, chapters)

  }
}
