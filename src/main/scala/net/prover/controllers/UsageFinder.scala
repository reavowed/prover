package net.prover.controllers

import net.prover.controllers.models.LinkSummary
import net.prover.entries.{ChapterWithContext, EntryWithContext, GlobalContext}
import net.prover.model._
import net.prover.model.entries.ChapterEntry
import net.prover.theorems.GetReferencedInferences

import scala.util.Try

case class UsagesForChapter(bookTitle: String, chapterTitle: String, theoremUsages: Seq[TheoremUsage])
case class TheoremUsage(link: LinkSummary, inferenceIds: Set[String])

trait UsageFinder {
  def bookService: BookService

  def getInferenceUsages(entry: ChapterEntry): Seq[UsagesForChapter] = {
    val allInferences = entry.inferences.toSet[Inference]
    def getInferenceLinks(chapterWithContext: ChapterWithContext): Seq[TheoremUsage] = {
      for {
        theoremWithContext <- chapterWithContext.theoremsWithContexts
        usedInferences = GetReferencedInferences(theoremWithContext).intersect(allInferences)
        if usedInferences.nonEmpty
      } yield TheoremUsage(LinkSummary(theoremWithContext), usedInferences.map(_.id))
    }
    for {
      bookWithContext <- bookService.globalContext.booksWithContexts
      chapterWithContext <- bookWithContext.chaptersWithContexts
      inferenceLinks = getInferenceLinks(chapterWithContext)
      if inferenceLinks.nonEmpty
    } yield UsagesForChapter(chapterWithContext.bookWithContext.book.title, chapterWithContext.chapter.title, inferenceLinks)
  }

  def checkNoUsages(entriesPotentiallyUsing: Iterable[EntryWithContext], entriesPotentiallyBeingUsed: Iterable[EntryWithContext]): Try[Any] = {
    findUsage(entriesPotentiallyUsing, entriesPotentiallyBeingUsed)
      .badRequestIfDefined { case (usedEntry, entryUsing) => s"""Entry "${entryUsing.entry.name}" depends on "${usedEntry.entry.name}"""" }
  }

  def findUsage(entriesPotentiallyUsing: Iterable[EntryWithContext], entriesPotentiallyBeingUsed: Iterable[EntryWithContext]): Option[(EntryWithContext, EntryWithContext)] = {
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
