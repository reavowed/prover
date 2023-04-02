package net.prover.controllers

import net.prover.books.model.Book
import net.prover.controllers.models.LinkSummary
import net.prover.entries.{ChapterWithContext, EntryWithContext, GlobalContext}
import net.prover.model._
import net.prover.model.entries.ChapterEntry
import net.prover.theorems.GetReferencedInferences

import scala.util.Try

trait UsageFinder {
  def bookService: BookService

  def getInferenceUsages(entry: ChapterEntry): Seq[(String, String, Seq[(LinkSummary, Set[String])])] = {
    val allInferences = entry.inferences.toSet[Inference]
    def getInferenceLinks(chapterWithContext: ChapterWithContext): Seq[(LinkSummary, Set[String])] = {
      for {
        theoremWithContext <- chapterWithContext.theoremsWithContexts
        usedInferences = GetReferencedInferences(theoremWithContext).intersect(allInferences)
        if usedInferences.nonEmpty
      } yield (LinkSummary(theoremWithContext), usedInferences.map(_.id))
    }
    for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      inferenceLinks = getInferenceLinks(chapterWithContext)
      if inferenceLinks.nonEmpty
    } yield (chapterWithContext.bookWithContext.book.title, chapterWithContext.chapter.title, inferenceLinks)
  }

  def checkNoUsages(entriesPotentiallyUsing: Seq[EntryWithContext], entriesPotentiallyBeingUsed: Seq[EntryWithContext]): Try[Any] = {
    findUsage(entriesPotentiallyUsing, entriesPotentiallyBeingUsed)
      .badRequestIfDefined { case (usedEntry, entryUsing) => s"""Entry "${entryUsing.entry.name}" depends on "${usedEntry.entry.name}"""" }
  }

  def findUsage(entriesPotentiallyUsing: Seq[EntryWithContext], entriesPotentiallyBeingUsed: Seq[EntryWithContext]): Option[(EntryWithContext, EntryWithContext)] = {
    entriesPotentiallyUsing
      .mapFind { entryUsing =>
        GetReferencedInferences(entryUsing).mapFind(referencedInference => entriesPotentiallyBeingUsed.find(_.entry.inferences.contains(referencedInference)).map(entryUsing -> _)) orElse
        entryUsing.entry.referencedEntries
          .mapFind(referencedDefinition => entriesPotentiallyBeingUsed.find(_.entry == referencedDefinition).map(entryUsing -> _))
      }
  }

  def findUsage(globalContext: GlobalContext, entry: EntryWithContext): Option[(EntryWithContext, EntryWithContext)] = {
    findUsage(globalContext.allEntries, Seq(entry))
  }
}
