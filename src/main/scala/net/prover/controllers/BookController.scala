package net.prover.controllers

import net.prover.JsonMapping
import net.prover.controllers.BookController.NewTheoremModel
import net.prover.controllers.models.ChapterProps
import net.prover.exceptions.BadRequestException
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.entries._
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext}
import net.prover.views._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation._

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import scala.xml.Unparsed

@RestController
@RequestMapping(Array("/books"))
class BookController @Autowired() (bookService: BookService) {
  case class BooksProps(bookKeys: Seq[Book.Key])
  @GetMapping(value = Array(""), produces = Array("text/html;charset=UTF-8"))
  def get: ResponseEntity[_] = {
    Try(createReactView("Books", BooksProps(bookService.books.map(_.key)))).toResponseEntity
  }

  @GetMapping(value = Array("reloadFromDisk"))
  def reloadFromDisk(): Unit = {
    bookService.reload().toResponseEntity
  }

  case class ChapterSummary(title: String, chapterKey: Chapter.Key, summary: String)
  case class BookProps(title: String, bookKey: Book.Key, chapters: Seq[ChapterSummary], previous: Option[Book.Key], next: Option[Book.Key])
  @GetMapping(value = Array("/{bookKey}"), produces = Array("text/html;charset=UTF-8"))
  def getBook(@PathVariable("bookKey") bookKey: String): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- books.find(_.key.value == bookKey).orNotFound(s"Book $bookKey")
    } yield {
      val index = books.indexOf(book)
      val previous = if (index > 0) Some(books(index - 1).key) else None
      val next = if (index < books.length - 1) Some(books(index + 1).key) else None
      createReactView("Book", BookProps(book.title, book.key, book.chapters.map(c => ChapterSummary(c.title, c.key, c.summary)), previous, next))
    }).toResponseEntity
  }

  @GetMapping(value = Array("/{bookKey}/{chapterKey}"), produces = Array("text/html;charset=UTF-8"))
  def getChapter(@PathVariable("bookKey") bookKey: String, @PathVariable("chapterKey") chapterKey: String): ResponseEntity[_] = {
    (for {
      book <- bookService.books.find(_.key.value == bookKey).orNotFound(s"Book $bookKey")
      chapter <- book.chapters.find(_.key.value == chapterKey).orNotFound(s"Chapter $chapterKey")
    } yield {
      val parsingContext = getChapterParsingContext(book, chapter)
      createReactView(
        "Chapter",
        ChapterProps(chapter, book),
        Map(
          "definitions" -> (parsingContext.statementDefinitions ++ parsingContext.termDefinitions).filter(_.componentTypes.nonEmpty).map(d => d.symbol -> d).toMap,
          "shorthands" -> book.displayContext.displayShorthands))
    }).toResponseEntity
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
        ChapterEntry.Key.Standalone(newTheoremDefininition.name, Chapter.getNextKey(chapter.entries, newTheoremDefininition.name), chapter.key),
        premises,
        conclusion,
        Seq(Step.Target(conclusion, StepContext(premises.map(_.provenStatement), Nil))),
        RearrangementType.NotRearrangement)

      val existingTheoremOption = parsingContext.inferences.find(_.id == newTheorem.id)
      existingTheoremOption match {
        case Some(_) =>
          Failure(BadRequestException("An inference with these premises and conclusion already exists"))
        case None =>
          Success(newTheorem)
      }
    }.map{ case (_, book, chapter) => ChapterProps(chapter, book) }.toResponseEntity
  }

  case class TheoremProps(theorem: Theorem, previousEntry: Option[ChapterEntry.Key], nextEntry: Option[ChapterEntry.Key], usages: Seq[(Book, Chapter, Seq[Theorem])])
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
            val parsingContext = getChapterParsingContext(book, chapter)
            createReactView(
              "Theorem",
              TheoremProps(theorem, previous.map(_.key), next.map(_.key), getUsages(theorem, books)),
              Map(
                "definitions" -> (parsingContext.statementDefinitions ++ parsingContext.termDefinitions).filter(_.componentTypes.nonEmpty).map(d => d.symbol -> d).toMap,
                "shorthands" -> book.displayContext.displayShorthands))
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
    def deleteEntry(chapterEntry: ChapterEntry, chapter: Chapter, books: Seq[Book]): Try[Chapter] = {
      chapterEntry match {
        case inference: Inference if getUsages(inference, books).isEmpty =>
          Success(chapter.copy(entries = chapter.entries.filter(_ != inference)))
        case _: Inference =>
          Failure(BadRequestException("Cannot delete inference with usages"))
        case _ =>
          Failure(BadRequestException("Deleting non-inference entries not yet supported"))
      }
    }

    bookService.modifyChapter(bookKey, chapterKey){ (books, _, chapter) =>
      for {
        entry <- chapter.entries.find(_.key.value == entryKey).orNotFound(s"Entry $entryKey")
        updatedChapter <- deleteEntry(entry, chapter, books)
      } yield (updatedChapter, ())
    }.map{ case (_, book, chapter, _) => ChapterProps(chapter, book) }.toResponseEntity
  }

  @PutMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}/shorthand"), produces = Array("application/json;charset=UTF-8"))
  def editShorthand(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newShorthand: String
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ExpressionDefinition, ChapterProps](bookKey, chapterKey, entryKey, (_, _, _, definition) =>
      Success(definition.withShorthand(Option(newShorthand).filter(_.nonEmpty)))
    ).map{ case (_, book, chapter, _) => ChapterProps(chapter, book) }.toResponseEntity
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

  private def createReactView(viewName: String, props: AnyRef, globals: Map[String, AnyRef] = Map.empty): String = {
    val initScript = (globals.map { case (name, value) => s"window.$name = ${JsonMapping.toString(value)};" }.toSeq
      :+ s"App.render(App.$viewName, ${JsonMapping.toString(props)});"
    ).mkString("\n")
    <html>
      <head>
        <title>Prover</title>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous" />
        <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.8.0/css/all.css" integrity="sha384-Mmxa0mLqhmOeaE8vgOSbKacftZcsNYDjQzuCOm6D02luYSzBG8vpaOykv9lFQ51Y" crossorigin="anonymous" />
      </head>
      <body>
        <script src="http://localhost:8081/js/bundle.js"></script>
        <script type="text/javascript">{Unparsed(initScript)}</script>
      </body>
    </html>.toString()
  }
}

object BookController {
  val logger: Logger = LoggerFactory.getLogger(BookController.getClass)

  case class NewTheoremModel(
    name: String,
    premises: Seq[String],
    conclusion: String)
}
