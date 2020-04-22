package net.prover.controllers

import net.prover.exceptions.InferenceReplacementException
import net.prover.model.definitions.Definitions
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.{Book, Chapter, EntryContext, Inference, ProvingContext}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RequestParam, RestController}

import scala.util.{Failure, Success}

@RestController
@RequestMapping(Array("/"))
class OperationsController @Autowired() (val bookService: BookService) extends BookModification with ReactViews {

  private def updateEntries[TMetadata](initial: TMetadata, f: Definitions => (Book, Chapter, ChapterEntry, EntryContext, TMetadata) => (ChapterEntry, TMetadata)): Unit = {
    println("Beginning update operation")
    bookService.modifyBooks[Identity] { (books, definitions) =>
      val update = f(definitions)
      def modifyEntry(book: Book, chapter: Chapter, tuple: (TMetadata, EntryContext), chapterEntry: ChapterEntry): ((TMetadata, EntryContext), ChapterEntry) = {
        val (metadata, entryContext) = tuple
        val (newEntry, newMetadata) = update(book, chapter, chapterEntry, entryContext, metadata)
        ((newMetadata, entryContext.addEntry(newEntry)), newEntry)
      }
      def modifyChapter(book: Book, tuple: (TMetadata, EntryContext), chapter: Chapter): ((TMetadata, EntryContext), Chapter) = {
        println("  - " + chapter.title)
        chapter.entries.mapFold(tuple)(modifyEntry(book, chapter, _, _))
          .mapRight(entries => chapter.copy(entries = entries))
      }
      def modifyBook(changedInferences: TMetadata, previousBooks: Seq[Book], book: Book): (TMetadata, Book) = {
        println("- " + book.title)
        val entryContext = EntryContext.forBookExclusive(previousBooks, book)
        book.chapters.mapFold((changedInferences, entryContext))(modifyChapter(book, _, _))
          .mapRight(chapters => book.copy(chapters = chapters))
          .mapLeft(_._1)
      }
      books.mapFoldWithPrevious(initial)(modifyBook)._2
    }
    println("Update operation complete")
  }

  @GetMapping(value = Array("clearInferencesUsingOldFunction"))
  def clearInferencesUsingOldFunction(): Unit = {
    updateEntries[Seq[Inference]](Nil, definitions => {
      val oldFunctionDefinition = definitions.rootEntryContext.typeDefinitions.find(_.symbol == "oldFunction").get
      (_, _, chapterEntry, _, inferencesToClear) => {
        val referencedEntries = chapterEntry match {
          case theorem: Theorem =>
            (theorem.premises :+ theorem.conclusion).flatMap(_.referencedDefinitions).map(_.associatedChapterEntry).toSet
          case other =>
            other.referencedEntries
        }
        val updatedInferences = if (referencedEntries.contains(oldFunctionDefinition)) inferencesToClear ++ chapterEntry.inferences else inferencesToClear
        val updatedEntry = chapterEntry.asOptionalInstanceOf[Theorem] match {
          case Some(theorem) =>
            inferencesToClear.foldLeft(theorem)(_.clearInference(_))
          case _ =>
            chapterEntry
        }
        (updatedEntry, updatedInferences)
      }
    })
  }

  @GetMapping(value = Array("replaceInference"))
  def replaceInference(
    @RequestParam("old") oldInferenceId: String,
    @RequestParam("new") newInferenceId: String
  ): Unit = {
    updateEntries[Unit](Map.empty, definitions => {
      val oldInference = definitions.allInferences.find(_.id == oldInferenceId).get
      val newInference = definitions.allInferences.find(_.id == newInferenceId).get
      (book, chapter, chapterEntry, entryContext, _) => {
        val updated = chapterEntry.asOptionalInstanceOf[Theorem] match {
          case Some(theorem) =>
            val provingContext = ProvingContext(entryContext, definitions)
            theorem.replaceInference(oldInference, newInference, provingContext) match {
              case Success(theorem) =>
                theorem.recalculateReferences(provingContext)._1
              case Failure(InferenceReplacementException.AtTheorem(message, stepPath, proofIndex, theoremName)) =>
                throw InferenceReplacementException.AtBook(message, stepPath, proofIndex, theoremName, chapter.title, book.title)
              case Failure(e) =>
                throw e
            }
          case _ =>
            chapterEntry
        }
        (updated, ())
      }
    })
  }

  @GetMapping(value = Array("clearInference"))
  def clearInference(
    @RequestParam("id") inferenceId: String
  ): Unit = {
    updateEntries[Unit](Map.empty, definitions => {
      val inference = definitions.allInferences.find(_.id == inferenceId).get
      (_, _, chapterEntry, _, _) => {
        val updated = chapterEntry.asOptionalInstanceOf[Theorem] match {
          case Some(theorem) =>
            theorem.clearInference(inference)
          case _ =>
            chapterEntry
        }
        (updated, ())
      }
    })
  }
}
