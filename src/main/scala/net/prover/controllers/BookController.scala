package net.prover.controllers

import net.prover.JsonMapping
import net.prover.controllers.BookController.NewTheoremModel
import net.prover.controllers.models.ChapterProps._
import net.prover.controllers.models.{ChapterProps, LinkSummary}
import net.prover.exceptions.BadRequestException
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.entries._
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}
import scala.xml.Unparsed

@RestController
@RequestMapping(Array("/books"))
class BookController @Autowired() (val bookService: BookService) extends BookModification {

  private def getChapterProps(books: Seq[Book], book: Book, bookKey: String, chapter: Chapter, chapterKey: String): ChapterProps = {
    val chaptersWithKeys = getChaptersWithKeys(book)
    val index = chaptersWithKeys.findIndexWhere(_._1 == chapter).getOrElse(throw new Exception("Book somehow didn't exist"))
    val previous = chaptersWithKeys.lift(index - 1).map { case (c, key) => LinkSummary(c.title, getChapterUrl(bookKey, key)) }
    val next = chaptersWithKeys.lift(index + 1).map { case (c, key) => LinkSummary(c.title, getChapterUrl(bookKey, key)) }

    val entrySummaries = getEntriesWithKeys(chapter).mapCollect{
      case (axiom: entries.Axiom, key) =>
        import axiom._
        Some(AxiomPropsForChapter(name, getEntryUrl(bookKey, chapterKey, key), premises, conclusion))
      case (theorem: entries.Theorem, key) =>
        import theorem._
        Some(TheoremPropsForChapter(name, getEntryUrl(bookKey, chapterKey, key), premises, conclusion))
      case (statementDefinition: entries.StatementDefinition, key) =>
        import statementDefinition._
        Some(StatementDefinitionPropsForChapter(defaultValue, getEntryUrl(bookKey, chapterKey, key), shorthand, definingStatement))
      case (termDefinition: entries.TermDefinition, key) =>
        import termDefinition._
        Some(TermDefinitionPropsForChapter(defaultValue, getEntryUrl(bookKey, chapterKey, key), shorthand, definingStatement, premises))
      case (comment: entries.Comment, key) =>
        import comment._
        Some(CommentPropsForChapter(text, key))
      case _ =>
        None
    }
    ChapterProps(
      chapter.title,
      getChapterUrl(bookKey, chapterKey),
      LinkSummary(book.title, getBookUrl(bookKey)),
      chapter.summary,
      entrySummaries,
      previous,
      next)
  }

  case class BooksProps(books: Seq[LinkSummary])
  @GetMapping(value = Array(""), produces = Array("text/html;charset=UTF-8"))
  def get: ResponseEntity[_] = {
    val bookProps = BooksProps(getBooksWithKeys(bookService.books).map { case (book, key) => LinkSummary(book.title, getBookUrl(key)) })
    Try(createReactView("Books", bookProps)).toResponseEntity
  }

  @GetMapping(value = Array("reloadFromDisk"))
  def reloadFromDisk(): Unit = {
    bookService.reload().toResponseEntity
  }

  case class ChapterSummary(title: String, url: String, summary: String)
  case class BookProps(title: String, url: String, chapters: Seq[ChapterSummary], previous: Option[LinkSummary], next: Option[LinkSummary])
  @GetMapping(value = Array("/{bookKey}"), produces = Array("text/html;charset=UTF-8"))
  def getBook(@PathVariable("bookKey") bookKey: String): ResponseEntity[_] = {
    val booksWithKeys = getBooksWithKeys(bookService.books)
    (for {
      book <- findBook(booksWithKeys, bookKey)
    } yield {
      val index = booksWithKeys.findIndexWhere(_._1 == book).getOrElse(throw new Exception("Book somehow didn't exist"))
      val previous = booksWithKeys.lift(index - 1).map { case (b, key) => LinkSummary(b.title, key) }
      val next = booksWithKeys.lift(index + 1).map { case (b, key) => LinkSummary(b.title, key) }
      val chapterSummaries = getChaptersWithKeys(book).map { case (c, key) => ChapterSummary(c.title, getChapterUrl(bookKey, key), c.summary) }
      createReactView("Book", BookProps(book.title, getBookUrl(bookKey), chapterSummaries, previous, next))
    }).toResponseEntity
  }

  @GetMapping(value = Array("/{bookKey}/{chapterKey}"), produces = Array("text/html;charset=UTF-8"))
  def getChapter(@PathVariable("bookKey") bookKey: String, @PathVariable("chapterKey") chapterKey: String): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chaptersWithKeys = getChaptersWithKeys(book)
      chapter <- findChapter(chaptersWithKeys, chapterKey)
    } yield {
      val entryContext = EntryContext.forChapterInclusive(books, book, chapter)
      createReactView(
        "Chapter",
        getChapterProps(books, book, bookKey, chapter, chapterKey),
        Map(
          "definitions" -> getDefinitionSummaries(entryContext),
          "displayShorthands" -> entryContext.availableEntries.ofType[DisplayShorthand],
          "definitionShorthands" -> getDefinitionShorthands(entryContext)))
    }).toResponseEntity
  }

  case class InferenceProps(inference: Inference.Entry, previousEntry: Option[LinkSummary], nextEntry: Option[LinkSummary], usages: Seq[(Book, Chapter, Seq[Theorem])])
  @GetMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}"), produces = Array("text/html;charset=UTF-8"))
  def getEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      entriesWithKeys = getEntriesWithKeys(chapter).mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[ChapterEntry.Standalone]))
      entry <- findEntry[ChapterEntry](entriesWithKeys, entryKey)
      (viewName, baseProps: Map[String, AnyRef], baseGlobals: Map[String, AnyRef]) <- entry match {
        case axiom: Axiom =>
          Success(("Axiom", Map("axiom" -> axiom), Map.empty))
        case theorem: Theorem =>
          Success(("Theorem", Map("theorem" -> theorem), Map("inferences" -> getInferenceLinks(theorem.referencedInferenceIds))))
        case statementDefinition: StatementDefinition =>
          Success(("StatementDefinition", Map("definition" -> statementDefinition), Map.empty))
        case termDefinition: TermDefinition =>
          Success(("TermDefinition", Map("definition" -> termDefinition), Map.empty))
        case _ =>
          Failure(BadRequestException(s"Cannot view ${entry.getClass.getSimpleName}"))
      }
    } yield {
      val entryContext = EntryContext.forEntry(books, book, chapter, entry).addEntry(entry)
      val index = entriesWithKeys.findIndexWhere(_._1 == entry).getOrElse(throw new Exception("Book somehow didn't exist"))
      val previous = entriesWithKeys.lift(index - 1).map { case (c, key) => LinkSummary(c.title, key) }
      val next = entriesWithKeys.lift(index + 1).map { case (c, key) => LinkSummary(c.title, key) }
      createReactView(
        viewName,
        baseProps ++ Map(
          "url" -> getEntryUrl(bookKey, chapterKey, entryKey),
          "bookLink" -> LinkSummary(book.title, getBookUrl(bookKey)),
          "chapterLink" -> LinkSummary(chapter.title, getChapterUrl(bookKey, chapterKey)),
          "previous" -> previous,
          "next" -> next,
          "usages" -> getUsages(entry, books)),
        baseGlobals ++ Map(
          "definitions" -> getDefinitionSummaries(entryContext),
          "displayShorthands" -> entryContext.availableEntries.ofType[DisplayShorthand],
          "transitivityInferences" -> entryContext.transitivityInferences.map { case (d, i) => d.symbol -> i.id },
          "definitionShorthands" -> getDefinitionShorthands(entryContext)))
    }).toResponseEntity
  }

  @PostMapping(value = Array("/{bookKey}/{chapterKey}/theorems"), produces = Array("application/json;charset=UTF-8"))
  def createTheorem(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTheoremDefininition: NewTheoremModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      for {
        premises <- newTheoremDefininition.premises.mapWithIndex((str, index) => Statement.parser.parseFromString(str, s"premise ${index + 1}").recoverWithBadRequest).traverseTry
        conclusion <- Statement.parser.parseFromString(newTheoremDefininition.conclusion, "conclusion").recoverWithBadRequest
        newTheorem = Theorem(
          newTheoremDefininition.name,
          premises,
          conclusion,
          Seq(Step.Target(conclusion)),
          RearrangementType.NotRearrangement)
        existingTheoremOption = entryContext.inferences.find(_.id == newTheorem.id)
        _ <- existingTheoremOption match {
          case Some(_) =>
            Failure(BadRequestException("An inference with these premises and conclusion already exists"))
          case None =>
            Success(newTheorem)
        }
      } yield newTheorem
    }.map{ case (books, book, chapter) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}/move"), produces = Array("application/json;charset=UTF-8"))
  def moveEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestParam("direction") direction: String
  ): ResponseEntity[_] = {
    def tryMove(entry: ChapterEntry, previousEntries: Seq[ChapterEntry], nextEntries: Seq[ChapterEntry]): Try[Seq[ChapterEntry]] = {
      direction match {
        case "up" =>
          for {
            (earlierEntries, precedingEntry) <- :+.unapply(previousEntries).orBadRequest("Entry was first in chapter")
            _ <- precedingEntry.asOptionalInstanceOf[Inference].filter(i => entry.referencedInferenceIds.contains(i.id)).badRequestIfDefined("Entry depends on previous one")
            _ <- precedingEntry.asOptionalInstanceOf[ExpressionDefinition].filter(entry.referencedDefinitions.contains).badRequestIfDefined("Entry depends on previous one")
          } yield earlierEntries ++ Seq(entry, precedingEntry) ++ nextEntries
        case "down" =>
          for {
            (nextEntry, lastEntries) <- +:.unapply(nextEntries).orBadRequest("Entry was last in chapter")
            _ <- entry.asOptionalInstanceOf[Inference].filter(i => nextEntry.referencedInferenceIds.contains(i.id)).badRequestIfDefined("Next entry depends on this one")
            _ <- entry.asOptionalInstanceOf[ExpressionDefinition].filter(nextEntry.referencedDefinitions.contains).badRequestIfDefined("Next entry depends on this one")
          } yield previousEntries ++ Seq(nextEntry, entry) ++ lastEntries
      }
    }
    modifyChapter(bookKey, chapterKey, (_, _, chapter) => {
      for {
        (previousEntries, entry, nextEntries) <- getEntriesWithKeys(chapter).splitWhere(_._2 == entryKey).orNotFound(s"Entry $entryKey")
        updatedEntries <- tryMove(entry._1, previousEntries.map(_._1), nextEntries.map(_._1))
      } yield (chapter.copy(entries = updatedEntries), ())
    }).map{ case (books, book, chapter, _) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
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

    modifyChapter(bookKey, chapterKey, (books, _, chapter) =>
      for {
        entry <- findEntry[ChapterEntry](chapter, entryKey)
        updatedChapter <- deleteEntry(entry, chapter, books)
      } yield (updatedChapter, ())
    ).map{ case (books, book, chapter, _) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PutMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}/shorthand"), produces = Array("application/json;charset=UTF-8"))
  def editShorthand(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newShorthand: String
  ): ResponseEntity[_] = {
    modifyEntry[ExpressionDefinition, Unit](bookKey, chapterKey, entryKey, (_, _, _, definition) =>
      Success(definition.withShorthand(Option(newShorthand).filter(_.nonEmpty)) -> ())
    ).map{ case (books, book, chapter, _, _) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  case class DefinitionSummary(symbol: String, baseFormatString: String, requiresBrackets: Boolean, numberOfBoundVariables: Int, structureType: Option[String])
  private def getDefinitionSummaries(entryContext: EntryContext) = {
    entryContext.availableEntries.ofType[ExpressionDefinition]
      .filter(_.componentTypes.nonEmpty)
      .map(d => d.symbol -> DefinitionSummary(d.symbol, d.format.baseFormatString, d.format.requiresBrackets, d.boundVariableNames.length, d.asOptionalInstanceOf[StatementDefinition].flatMap(_.structureType).map(_.serialized))).toMap
  }

  private def getDefinitionShorthands(entryContext: EntryContext): Map[String, String] = {
    val shorthandsFromDefinitions = entryContext.availableEntries.ofType[ExpressionDefinition].mapCollect(d => d.shorthand.map(_ -> d.symbol)).toMap
    val greekLetterShorthands = 'α'.to('ω')
      .map(c => Character.getName(c).splitByWhitespace().last.toLowerCase -> c.toString)
      .toMap
    shorthandsFromDefinitions ++ greekLetterShorthands
  }

  private def getUsages(entry: ChapterEntry, books: Seq[Book]): Seq[(String, String, Seq[LinkSummary])] = {
    val inferenceIds = entry.inferences.map(_.id).toSet
    for {
      (book, bookKey) <- getBooksWithKeys(bookService.books)
      (chapter, chapterKey) <- getChaptersWithKeys(book)
      theoremsWithKeys = getEntriesWithKeys(chapter)
        .mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[Theorem]))
        .filter(_._1.referencedInferenceIds.intersect(inferenceIds).nonEmpty)
      if theoremsWithKeys.nonEmpty
    } yield (book.title, chapter.title, theoremsWithKeys.map { case (theorem, key) => LinkSummary(theorem.name, getEntryUrl(bookKey, chapterKey, key))})
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
        <style>{".popover { max-width: 100%; }"}</style>
      </head>
      <body>
        <script src="http://localhost:8081/node_modules/react/umd/react.development.js"></script>
        <script src="http://localhost:8081/node_modules/react-dom/umd/react-dom.development.js"></script>
        <script src="http://localhost:8081/bundle.js"></script>
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
