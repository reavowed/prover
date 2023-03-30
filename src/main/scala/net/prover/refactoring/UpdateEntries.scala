package net.prover.refactoring

import net.prover.books.management.{BookStateManager, UpdateBooks}
import net.prover.books.model.Book
import net.prover.entries.{BookWithContext, ChapterWithContext, EntryWithContext, GlobalContext}
import net.prover.model._
import net.prover.model.entries.ChapterEntry
import scalaz.Id.Id

object UpdateEntries {
  def apply(
    getUpdateOperation: GlobalContext => EntryWithContext => ChapterEntry)(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    apply[Unit]((), globalContext => { case (entryWithContext, _) =>
      (getUpdateOperation(globalContext)(entryWithContext), ())
    })
  }

  def apply[TMetadata](
    initial: TMetadata,
    getUpdateOperation: GlobalContext => (EntryWithContext, TMetadata) => (ChapterEntry, TMetadata))(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    println("Beginning update operation")
    UpdateBooks[Id](globalContext => {
      val updateOperation = getUpdateOperation(globalContext)
      def modifyBook(metadata: TMetadata, previousBooks: Seq[Book], bookWithContext: BookWithContext): (TMetadata, Book) = {
        import bookWithContext._
        println("- " + book.title)
        val (newMetadata, newChapters) = book.chapters.mapFoldWithPrevious[TMetadata, Chapter](metadata) { case (metadata, updated, chapter) =>
          modifyChapter(metadata, bookWithContext.copy(globalContext = GlobalContext(previousBooks.toList, definitions), book = book.setChapters((updated :+ chapter).toList)).getChapter(chapter))
        }
        (newMetadata, book.setChapters(newChapters.toList))
      }

      def modifyChapter(metadata: TMetadata, chapterWithContext: ChapterWithContext): (TMetadata, Chapter) = {
        import chapterWithContext._
        println("  - " + chapter.title)
        val (newMetadata, newEntries) = chapter.entries.mapFoldWithPrevious[TMetadata, ChapterEntry](metadata) { case (metadata, updated, entry) =>
          updateOperation(chapterWithContext.copy(chapter = chapter.copy(entries = updated :+ entry)).getEntry(entry), metadata).swap
        }
        (newMetadata, chapter.copy(entries = newEntries))
      }

      globalContext.booksWithContexts.mapFoldWithPrevious(initial)(modifyBook)._2
    })
    println("Update operation complete")
  }
}
