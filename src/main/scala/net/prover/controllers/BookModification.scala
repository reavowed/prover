package net.prover.controllers

import net.prover.books.model.Book
import net.prover.controllers.models.{InsertionAndReplacementProps, LinkSummary, PathData, ProofUpdateProps, StepInsertionProps, StepReplacementProps}
import net.prover.exceptions.NotFoundException
import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.{Step, StepProvingContext, StepReference, SubstitutionContext}
import net.prover.proving.stepReplacement.AddTargetsBeforeChain

import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Try}

trait BookModification {
  def bookService: BookService

  implicit def toIndexes(pathData: PathData): Seq[Int] = pathData.indexes

  protected def findInference(inferenceId: String)(implicit stepProvingContext: StepProvingContext): Try[Inference.Summary] = {
    stepProvingContext.provingContext.entryContext.allInferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }

  def getInferenceUsages(entry: ChapterEntry, books: Seq[Book]): Seq[(String, String, Seq[(LinkSummary, Set[String])])] = {
    val allInferenceIds = entry.inferences.map(_.id).toSet
    def getInferenceLinks(bookKey: String, chapterKey: String, entries: Seq[(ChapterEntry, String)]): Seq[(LinkSummary, Set[String])] = {
      for {
        (entry, key) <- entries
        theorem <- entry.asOptionalInstanceOf[Theorem].toSeq
        usedInferenceIds = theorem.referencedInferenceIds.intersect(allInferenceIds)
        if usedInferenceIds.nonEmpty
      } yield (LinkSummary(theorem.name, BookService.getEntryUrl(bookKey, chapterKey, key)), usedInferenceIds)
    }
    for {
      (book, bookKey) <- bookService.getBooksWithKeys
      (chapter, chapterKey) <- BookService.getChaptersWithKeys(book)
      inferenceLinks = getInferenceLinks(bookKey, chapterKey, BookService.getEntriesWithKeys(chapter))
      if inferenceLinks.nonEmpty
    } yield (book.title, chapter.title, inferenceLinks)
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
