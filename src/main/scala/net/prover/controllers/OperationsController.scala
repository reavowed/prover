package net.prover.controllers

import net.prover.model.definitions.{Definitions, TermDefinition}
import net.prover.model.entries.{ChapterEntry, Theorem, WritingShorthand}
import net.prover.model.{Book, Chapter, EntryContext, ProvingContext}
import net.prover.exceptions.InferenceReplacementException
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

  @GetMapping(value = Array("replaceZero"))
  def replaceZero(): Unit = {
    updateEntries[Map[String, String]](Map.empty, _ => (_, _, chapterEntry, entryContext, changedInferences) => {
      if (chapterEntry.asOptionalInstanceOf[TermDefinition].exists(_.symbol == "0_ℤ")) {
        (WritingShorthand.parser(entryContext).parseFromString("apply ⍳_ℤ 0 as 0_ℤ", ""), changedInferences)
      } else {
        val serializedEntry = chapterEntry.serializedLines.mkString("\n")
        val replacedSerializedEntry = changedInferences.foldLeft(serializedEntry) { case (currentSerializedEntry, (oldId, newId)) =>
          currentSerializedEntry.replace(oldId, newId)
        }
        val newEntry = Chapter.chapterEntryParser(entryContext).parseFromString(replacedSerializedEntry, chapterEntry.name).get
        val updatedInferences = changedInferences ++ chapterEntry.inferences.map(_.id).zip(newEntry.inferences.map(_.id)).filter { case (o, n) => o != n }.toMap
        (newEntry, updatedInferences)
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
