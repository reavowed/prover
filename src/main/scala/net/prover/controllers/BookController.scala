package net.prover.controllers

import net.prover.books.keys.ListWithKeys
import net.prover.books.model.Book
import net.prover.controllers.BookController.ChapterDefinition
import net.prover.controllers.models.LinkSummary
import net.prover.entries.{BookWithContext, ChapterWithContext}
import net.prover.model._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._
import scalaz.Id.Id

import scala.language.postfixOps
import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}"))
class BookController @Autowired() (val bookService: BookService) extends UsageFinder with ReactViews {

  case class ChapterSummary(title: String, url: String, summary: String)
  object ChapterSummary {
    def apply(chapterWithContext: ChapterWithContext): ChapterSummary = {
      import chapterWithContext.chapter
      ChapterSummary(chapter.title, BookService.getChapterUrl(chapterWithContext), chapter.summary)
    }
  }
  case class BookProps(title: String, url: String, chapters: Seq[ChapterSummary], previous: Option[LinkSummary], next: Option[LinkSummary])
  object BookProps {
    def apply(bookWithContext: BookWithContext): BookProps = {
      import bookWithContext.book
      import bookWithContext.globalContext
      val index = globalContext.booksWithContexts.findIndexWhere(_.book == book).getOrElse(throw new Exception("Book somehow didn't exist"))
      val previous = globalContext.booksWithContexts.lift(index - 1).map(LinkSummary(_))
      val next = globalContext.booksWithContexts.lift(index + 1).map(LinkSummary(_))
      val chapterSummaries = bookWithContext.chaptersWithContexts.map(ChapterSummary(_))
      BookProps(book.title, BookService.getBookUrl(bookWithContext), chapterSummaries, previous, next)
    }
  }

  @GetMapping(produces = Array("text/html;charset=UTF-8"))
  def getBook(@PathVariable("bookKey") bookKey: String): ResponseEntity[_] = {
    (for {
      bookWithContext <- bookService.findBook(bookKey)
    } yield {
      createReactView("Book", BookProps(bookWithContext))
    }).toResponseEntity
  }

  @PostMapping(value = Array("/chapters"), produces = Array("application/json;charset=UTF-8"))
  def createChapter(
    @PathVariable("bookKey") bookKey: String,
    @RequestBody chapterDefinition: ChapterDefinition
  ): ResponseEntity[_] = {
    bookService.modifyBook[Id](bookKey, bookWithContext => {
      val chapter = Chapter(chapterDefinition.title, chapterDefinition.summary, ListWithKeys.empty)
      val newBook = bookWithContext.book.addChapter(chapter)
      Success(newBook)
    }).map(BookProps(_)).toResponseEntity
  }

  @DeleteMapping(value = Array("/{chapterKey}"), produces = Array("application/json;charset=UTF-8"))
  def deleteChapter(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String
  ): ResponseEntity[_] = {
    bookService.modifyBook[Id](bookKey, bookWithContext => {
      import bookWithContext.book
      import bookWithContext.globalContext.booksWithKeys
      val entriesAfterInThisBook = bookWithContext.chaptersWithContexts.view
        .dropUntil { _.chapterKey == chapterKey }
        .flatMap(_.entriesWithContexts)
      val entriesInOtherBooks = bookWithContext.globalContext.booksWithContexts.filter(_.book != book).view
        .dropUntil{ _.bookKey == bookKey }
        .flatMap(_.chaptersWithContexts)
        .flatMap(_.entriesWithContexts)
      for {
        chapterWithContext <- bookWithContext.chaptersWithContexts.find(_.chapterKey == chapterKey) orNotFound s"Chapter $chapterKey"
        _ <- checkNoUsages(entriesAfterInThisBook ++ entriesInOtherBooks, chapterWithContext.entriesWithContexts)
      } yield {
        book.copy(chaptersWithKeys = book.chaptersWithKeys - chapterWithContext.chapter)
      }
    }).map(BookProps(_)).toResponseEntity
  }

  @PutMapping(value = Array("/{chapterKey}/index"), produces = Array("application/json;charset=UTF-8"))
  def moveChapterToIndex(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newIndex: Int
  ): ResponseEntity[_] = {
    def tryMove(chapter: ChapterWithContext, previousChapters: Seq[ChapterWithContext], nextChapters: Seq[ChapterWithContext]): Try[Seq[ChapterWithContext]] = {
      previousChapters.takeAndRemainingIfValid(newIndex).map { case (firstChapters, chaptersToMoveBefore) =>
        for {
          _ <- checkNoUsages(chapter.entriesWithContexts, chaptersToMoveBefore.flatMap(_.entriesWithContexts))
        } yield (firstChapters :+ chapter) ++ chaptersToMoveBefore ++ nextChapters
      } orElse nextChapters.takeAndRemainingIfValid(newIndex - previousChapters.length).map { case (chaptersToMoveAfter, lastChapters) =>
        for {
          _ <- checkNoUsages(chaptersToMoveAfter.flatMap(_.entriesWithContexts), chapter.entriesWithContexts)
        } yield (previousChapters ++ chaptersToMoveAfter :+ chapter) ++ lastChapters
      } orBadRequest "Invalid index" flatten
    }
    bookService.modifyBook[Id](bookKey, bookWithContext => {
      import bookWithContext._
      for {
        (previousChapters, chapter, nextChapters) <- bookWithContext.chaptersWithContexts.splitWhere(_.chapterKey == chapterKey).orNotFound(s"Chapter $chapterKey")
        updatedChapters <- tryMove(chapter, previousChapters, nextChapters)
      } yield book.setChapters(updatedChapters.map(_.chapter).toList)
    }).map(BookProps(_)).toResponseEntity
  }
}

object BookController {
  val logger: Logger = LoggerFactory.getLogger(BookController.getClass)

  case class ChapterDefinition(title: String, summary: String)
}
