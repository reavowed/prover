package net.prover.controllers

import net.prover.books.model.Book
import net.prover.controllers.models.LinkSummary
import net.prover.entries.ChapterWithContext
import net.prover.model._
import net.prover.model.entries.ChapterEntry

trait UsageFinder {
  def bookService: BookService

  def getInferenceUsages(entry: ChapterEntry): Seq[(String, String, Seq[(LinkSummary, Set[String])])] = {
    val allInferenceIds = entry.inferences.map(_.id).toSet
    def getInferenceLinks(chapterWithContext: ChapterWithContext): Seq[(LinkSummary, Set[String])] = {
      for {
        theoremWithContext <- chapterWithContext.theoremsWithContexts
        usedInferenceIds = theoremWithContext.theorem.referencedInferenceIds.intersect(allInferenceIds)
        if usedInferenceIds.nonEmpty
      } yield (LinkSummary(theoremWithContext), usedInferenceIds)
    }
    for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      inferenceLinks = getInferenceLinks(chapterWithContext)
      if inferenceLinks.nonEmpty
    } yield (chapterWithContext.bookWithContext.book.title, chapterWithContext.chapter.title, inferenceLinks)
  }

  def findUsage(entriesPotentiallyUsing: Seq[ChapterEntry], entriesPotentiallyBeingUsed: Seq[ChapterEntry]): Option[(ChapterEntry, ChapterEntry)] = {
    entriesPotentiallyUsing
      .mapFind { entryUsing =>
        entryUsing.referencedInferenceIds
          .mapFind(referencedInference => entriesPotentiallyBeingUsed.find(_.inferences.exists(_.id == referencedInference)).map(entryUsing -> _)) orElse
        entryUsing.referencedEntries
          .mapFind(referencedDefinition => entriesPotentiallyBeingUsed.find(_ == referencedDefinition).map(entryUsing -> _))
      }
  }

  def findUsage(books: Seq[Book], entry: ChapterEntry): Option[(ChapterEntry, ChapterEntry)] = {
    findUsage(books.view.flatMap(_.chapters).flatMap(_.entries), Seq(entry))
  }
}
