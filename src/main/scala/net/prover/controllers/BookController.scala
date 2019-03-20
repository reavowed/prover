package net.prover.controllers

import java.io.InputStreamReader

import javax.script.ScriptEngineManager
import jdk.nashorn.api.scripting.NashornScriptEngine
import net.prover.{JsonMapping, ServerSideRendering}
import net.prover.controllers.BookController.NewTheoremModel
import net.prover.exceptions.{BadRequestException, NotFoundException}
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.entries._
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext}
import net.prover.services.BookService
import net.prover.views._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation._

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

@RestController
@RequestMapping(Array("/books"))
class BookController @Autowired() (bookService: BookService) {
  @GetMapping(value = Array(""), produces = Array("text/html;charset=UTF-8"))
  def get = {
    try {
      BooksView(bookService.books).toString
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(value = Array("/{bookKey}"), produces = Array("text/html;charset=UTF-8"))
  def getBook(@PathVariable("bookKey") bookKey: String) = {
    try {
      bookService.books.find(_.key.value == bookKey) match {
        case Some(book) =>
          BookView(book).toString
        case None =>
          new ResponseEntity(HttpStatus.NOT_FOUND)
      }
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(value = Array("/{bookKey}/{chapterKey}"), produces = Array("text/html;charset=UTF-8"))
  def getChapter(@PathVariable("bookKey") bookKey: String, @PathVariable("chapterKey") chapterKey: String) = {
    try {
      (for {
        book <- bookService.books.find(_.key.value == bookKey)
        chapter <- book.chapters.find(_.key.value == chapterKey)
      } yield {
        val index = book.chapters.indexOf(chapter)
        val previous = if (index > 0) Some(book.chapters(index - 1)) else None
        val next = if (index < book.chapters.length - 1) Some(book.chapters(index + 1)) else None
        ChapterView(chapter, book, previous, next).toString
      }) getOrElse new ResponseEntity(HttpStatus.NOT_FOUND)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @PostMapping(value = Array("/{bookKey}/{chapterKey}/theorems"), produces = Array("application/json;charset=UTF-8"))
  def createTheorem(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTheoremDefininition: NewTheoremModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (_, book, chapter) =>
      implicit val parsingContext: ParsingContext = getChapterParsingContext(book, chapter)
      val premises = newTheoremDefininition.premises
        .mapWithIndex((str, index) => Statement.parser.parseFromString(str, s"premise ${index + 1}"))
        .mapWithIndex((statement, index) => Premise(statement, index)(false))
      val conclusion = Statement.parser.parseFromString(newTheoremDefininition.conclusion, "conclusion")
      val newTheorem = Theorem(
        newTheoremDefininition.name,
        ChapterEntry.Key.Standalone(Chapter.getNextKey(chapter.entries, newTheoremDefininition.name), chapter.key),
        premises,
        conclusion,
        Seq(Step.Target(conclusion, StepContext(premises.map(_.provenStatement), 0))),
        RearrangementType.NotRearrangement)

      val existingTheoremOption = parsingContext.inferences.find(_.id == newTheorem.id)
      existingTheoremOption match {
        case Some(_) =>
          Failure(BadRequestException("An inference with these premises and conclusion already exists"))
        case None =>
          Success((newTheorem, ()))
      }
    }.toResponseEntity
  }

  @GetMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}"), produces = Array("text/html;charset=UTF-8"))
  def getEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ) = {
    try {
      val books = bookService.books
      (for {
        book <- books.find(_.key.value == bookKey)
        chapter <- book.chapters.find(_.key.value == chapterKey)
        entries = chapter.entries.ofType[ChapterEntry.Standalone]
        entry <- entries.find(_.key.value == entryKey)
      } yield {
        val index = entries.indexOf(entry)
        val previous = if (index > 0) Some(entries(index - 1)) else None
        val next = if (index < entries.length - 1) Some(entries(index + 1)) else None
        entry match {
          case axiom: Axiom =>
            AxiomView(axiom, chapter, book, previous, next, getUsages(axiom, books)).toString
          case theorem: Theorem =>
            TheoremView(theorem, chapter, book, previous, next, getUsages(theorem, books)).toString
        }
      }) getOrElse new ResponseEntity(HttpStatus.NOT_FOUND)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error(s"Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}/new"), produces = Array("text/html;charset=UTF-8"))
  def getEntryNew(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ) = {
    try {
      val books = bookService.books
      (for {
        book <- books.find(_.key.value == bookKey)
        chapter <- book.chapters.find(_.key.value == chapterKey)
        entries = chapter.entries.ofType[ChapterEntry.Standalone]
        entry <- entries.find(_.key.value == entryKey)
      } yield {
        val index = entries.indexOf(entry)
        val previous = if (index > 0) Some(entries(index - 1)) else None
        val next = if (index < entries.length - 1) Some(entries(index + 1)) else None
        entry match {
          case axiom: Axiom =>
            AxiomView(axiom, chapter, book, previous, next, getUsages(axiom, books)).toString
          case theorem: Theorem =>
//            val content = ServerSideRendering.render("renderTheoremServer", theorem, previous, next, getUsages(theorem, books))
            NewTheoremView(theorem, chapter, book, previous, next, getUsages(theorem, books), "").toString
        }
      }) getOrElse new ResponseEntity(HttpStatus.NOT_FOUND)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error(s"Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @DeleteMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}"))
  def deleteEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ): ResponseEntity[_] = {
    bookService.modifyChapter(bookKey, chapterKey){ (books, _, chapter) =>
      chapter.entries.ofType[ChapterEntry.WithKey].find(_.key.value == entryKey) match {
        case Some(inference: Inference) if getUsages(inference, books).isEmpty =>
          Success((chapter.copy(entries = chapter.entries.filter(_ != inference)), ()))
        case Some(_: Inference) =>
          Failure(BadRequestException("Cannot delete inference with usages"))
        case Some(_) =>
          Failure(BadRequestException("Deleting non-inference entries not yet supported"))
        case None =>
          Failure(NotFoundException(s"Entry $entryKey"))
      }
    }.toResponseEntity
  }

  @PutMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}/shorthand"), produces = Array("application/json;charset=UTF-8"))
  def editShorthand(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newShorthand: String
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ExpressionDefinition, Unit](bookKey, chapterKey, entryKey) { (books, book, chapter, definition) =>
      Success((definition.withShorthand(Option(newShorthand).filter(_.nonEmpty)), ()))
    }.toResponseEntity
  }

  private def getUsages(inference: Inference, books: Seq[Book]): Seq[(Book, Chapter, Seq[Theorem])] = {
    for {
      book <- books
      chapter <- book.chapters
      theorems = chapter.entries.ofType[Theorem].filter(_.referencedInferenceIds.contains(inference.id))
      if theorems.nonEmpty
    } yield (book, chapter, theorems)
  }

  private def getChapterParsingContext(book: Book, chapter: Chapter): ParsingContext = {
      val chaptersSoFar = book.chapters.takeWhile(_ != chapter) :+ chapter
      ParsingContext(
        book.dependencies.transitive.inferences ++ chaptersSoFar.flatMap(_.inferences),
        book.dependencies.transitive.statementDefinitions ++ chaptersSoFar.flatMap(_.statementDefinitions),
        book.dependencies.transitive.termDefinitions ++ chaptersSoFar.flatMap(_.termDefinitions),
        book.termVariableNames.toSet,
        Nil)
  }
}

object BookController {
  val logger: Logger = LoggerFactory.getLogger(BookController.getClass)

  case class NewTheoremModel(
    name: String,
    premises: Seq[String],
    conclusion: String)
}
