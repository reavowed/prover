package net.prover

import net.prover.exceptions.{BadRequestException, NotFoundException}
import net.prover.model.entries.{StatementDefinition, TermDefinition, Theorem}
import net.prover.model.{Book, Chapter, ParsingContext}
import org.springframework.http.{HttpStatus, ResponseEntity}

import scala.util.{Failure, Success, Try}

package object controllers {
  implicit class TryToResponseEntityOps[T](t: Try[T]) {
    def toResponseEntity: ResponseEntity[_] = {
      t match {
        case Success(()) =>
          new ResponseEntity(HttpStatus.OK)
        case Success(value) =>
          new ResponseEntity[T](value, HttpStatus.OK)
        case Failure(NotFoundException(objectDescription)) =>
          new ResponseEntity(s"$objectDescription not found", HttpStatus.NOT_FOUND)
        case Failure(BadRequestException(message)) =>
          new ResponseEntity(message, HttpStatus.BAD_REQUEST)
        case Failure(e) =>
          throw e
      }
    }
  }
  implicit class OptionWithResponseExceptionOps[T](option: Option[T]) {
    def orException(e: Exception): Try[T] = {
      option match {
        case Some(t) => Success(t)
        case None => Failure(e)
      }
    }
    def orNotFound(objectDescription: String): Try[T] = orException(NotFoundException(objectDescription))
    def orBadRequest(message: String): Try[T] = orException(BadRequestException(message))
  }
  def getTheoremParsingContext(book: Book, chapter: Chapter, theorem: Theorem): ParsingContext = {
    val previousChapters = book.chapters.takeWhile(_ != chapter)
    val previousEntries = chapter.entries.takeWhile(_ != theorem)
    ParsingContext(
      book.dependencies.transitive.inferences ++ previousChapters.flatMap(_.inferences) ++ previousEntries.flatMap(_.inferences),
      book.dependencies.transitive.statementDefinitions ++ previousChapters.flatMap(_.statementDefinitions) ++ previousEntries.ofType[StatementDefinition],
      book.dependencies.transitive.termDefinitions ++ previousChapters.flatMap(_.termDefinitions) ++ previousEntries.ofType[TermDefinition],
      book.termVariableNames.toSet,
      Nil)
  }
}
