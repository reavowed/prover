package net.prover.refactoring

import net.prover.books.management.{BookStateManager, UpdateBooks}
import net.prover.controllers.Identity
import net.prover.entries.EntryWithContext
import net.prover.model.definitions.Definitions
import net.prover.model.{Book, Chapter, EntryContext}
import net.prover.model.entries.ChapterEntry

object UpdateEntries {
  def apply(
    getUpdateOperation: Definitions => EntryWithContext => ChapterEntry)(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    apply[Unit]((), definitions => {
      val updateOperation = getUpdateOperation(definitions)
      (entryWithContext, _) => (updateOperation(entryWithContext), ())
    })
  }

  def apply[TMetadata](
    initial: TMetadata,
    getUpdateOperation: Definitions => (EntryWithContext, TMetadata) => (ChapterEntry, TMetadata))(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    println("Beginning update operation")
    UpdateBooks[Identity]((books, definitions) => {
      val updateOperation = getUpdateOperation(definitions)

      def modifyBook(changedInferences: TMetadata, previousBooks: Seq[Book], book: Book): (TMetadata, Book) = {
        println("- " + book.title)
        val entryContext = EntryContext.forBookExclusive(previousBooks, book)
        book.chapters.mapFold((changedInferences, entryContext))(modifyChapter(book, _, _))
          .mapRight(chapters => book.copy(chapters = chapters))
          .mapLeft(_._1)
      }

      def modifyChapter(book: Book, tuple: (TMetadata, EntryContext), chapter: Chapter): ((TMetadata, EntryContext), Chapter) = {
        println("  - " + chapter.title)
        chapter.entries.mapFold(tuple)(modifyEntry(book, chapter, _, _))
          .mapRight(entries => chapter.copy(entries = entries))
      }

      def modifyEntry(book: Book, chapter: Chapter, tuple: (TMetadata, EntryContext), chapterEntry: ChapterEntry): ((TMetadata, EntryContext), ChapterEntry) = {
        val (metadata, entryContext) = tuple
        val (newEntry, newMetadata) = updateOperation(EntryWithContext(book, chapter, chapterEntry, entryContext), metadata)
        ((newMetadata, entryContext.addEntry(newEntry)), newEntry)
      }

      books.mapFoldWithPrevious(initial)(modifyBook)._2
    })
    println("Update operation complete")
  }
}
