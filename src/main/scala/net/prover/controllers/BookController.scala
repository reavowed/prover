package net.prover.controllers

import java.nio.file.{Files, Paths}

import net.prover.model.entries.Theorem
import net.prover.model.{Book, CachedProof, Chapter, Inference}
import org.slf4j.LoggerFactory
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation.{GetMapping, PathVariable, RequestMapping, RestController}

import scala.util.Try
import scala.util.control.NonFatal

@RestController
@RequestMapping(Array("/books"))
class BookController {
  def getBooks: Seq[Book] = Book.fromDirectory(Paths.get("books"), Paths.get("cache"))

  try {
    getBooks
  } catch {
    case NonFatal(e) =>
      BookController.logger.error("Error getting books", e)
  }

  @GetMapping(Array(""))
  def get = {
    try {
      new ResponseEntity[Seq[Book]](getBooks, HttpStatus.OK)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books\n{}", e.getMessage)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(Array("/{bookKey}"))
  def getBook(@PathVariable("bookKey") bookKey: String) = {
    try {
      getBooks.find(_.key == bookKey) match {
        case Some(book) =>
          new ResponseEntity[Book](book, HttpStatus.OK)
        case None =>
          new ResponseEntity(HttpStatus.NOT_FOUND)
      }
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books\n{}", e.getMessage)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(Array("/{bookKey}/{chapterKey}"))
  def getChapter(@PathVariable("bookKey") bookKey: String, @PathVariable("chapterKey") chapterKey: String) = {
    try {
      getBooks.find(_.key == bookKey).flatMap(_.chapters.find(_.key == chapterKey)) match {
        case Some(chapter) =>
          new ResponseEntity[Chapter](chapter, HttpStatus.OK)
        case None =>
          new ResponseEntity(HttpStatus.NOT_FOUND)
      }
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books\n{}", e.getMessage)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(Array("/{bookKey}/{chapterKey}/{inferenceKey}"))
  def getInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("inferenceKey") inferenceKey: String
  ) = {
    try {
      val books = getBooks
      val inferenceOption = for {
        book <- books.find(_.key == bookKey)
        chapter <- book.chapters.find(_.key == chapterKey)
        inference <- chapter.entries.ofType[Inference].find(_.keyOption.contains(inferenceKey))
      } yield {
        inference
      }
      inferenceOption match {
        case Some(inference) =>
          new ResponseEntity[InferenceWithUsages](InferenceWithUsages(inference, getUsages(inference, books)), HttpStatus.OK)
        case None =>
          new ResponseEntity(HttpStatus.NOT_FOUND)
      }
    } catch {
      case NonFatal(e) =>
        BookController.logger.error(s"Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  case class InferenceWithUsages(inference: Inference, bookUsages: Seq[BookUsages])
  case class BookUsages(bookTitle: String, bookKey: String, chapterUsages: Seq[ChapterUsages])
  case class ChapterUsages(chapterTitle: String, chapterKey: String, inferenceSummaries: Seq[Inference.Summary])

  private def getUsages(inference: Inference, books: Seq[Book]): Seq[BookUsages] = {
    books
      .map { book =>
        val chapterUsages = book.chapters
          .map { chapter =>
            val inferenceSummaries = chapter.entries.ofType[Theorem].filter(_.proof.referencedInferenceIds.contains(inference.id)).map(_.summary)
            ChapterUsages(chapter.title, chapter.key, inferenceSummaries)
          }
          .filter(_.inferenceSummaries.nonEmpty)
        BookUsages(book.title, book.key, chapterUsages)
      }
      .filter(_.chapterUsages.nonEmpty)
  }
}

object BookController {
  val logger = LoggerFactory.getLogger(BookController.getClass)
}
