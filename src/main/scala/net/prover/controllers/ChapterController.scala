package net.prover.controllers

import net.prover.controllers.ChapterController._
import net.prover.controllers.models.ChapterProps._
import net.prover.controllers.models.{ChapterProps, DefinitionSummary, LinkSummary, TypeDefinitionSummary}
import net.prover.exceptions.BadRequestException
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries._
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}"))
class ChapterController @Autowired() (val bookService: BookService) extends BookModification with ReactViews {

  private def getChapterProps(books: Seq[Book], book: Book, bookKey: String, chapter: Chapter, chapterKey: String): ChapterProps = {
    val chaptersWithKeys = getChaptersWithKeys(book)
    val index = chaptersWithKeys.findIndexWhere(_._1 == chapter).getOrElse(throw new Exception("Book somehow didn't exist"))
    val previous = chaptersWithKeys.lift(index - 1).map { case (c, key) => LinkSummary(c.title, getChapterUrl(bookKey, key)) }
    val next = chaptersWithKeys.lift(index + 1).map { case (c, key) => LinkSummary(c.title, getChapterUrl(bookKey, key)) }
    val entryContext = EntryContext.forChapterInclusive(books, book, chapter)

    val entrySummaries = getEntriesWithKeys(chapter)
      .map(_.mapRight(key => getEntryUrl(bookKey, chapterKey, key)))
      .mapCollect { case (entry, url) =>
        entry match {
          case axiom: entries.Axiom =>
            import axiom._
            Some(AxiomPropsForChapter(name, url, premises, conclusion))
          case theorem: entries.Theorem =>
            import theorem._
            Some(TheoremPropsForChapter(name, url, premises, conclusion, isComplete))
          case statementDefinition: entries.StatementDefinition =>
            import statementDefinition._
            Some(StatementDefinitionPropsForChapter(defaultValue, url, shorthand, definingStatement))
          case termDefinition: entries.TermDefinition =>
            import termDefinition._
            Some(TermDefinitionPropsForChapter(defaultValue, url, shorthand, definingStatement, premises))
          case typeDefinition: entries.TypeDefinition =>
            import typeDefinition._
            Some(TypeDefinitionPropsForChapter(symbol, url, defaultTermName, otherComponentTypes.map(_.name), definingStatement))
          case propertyDefinition: entries.PropertyDefinition =>
            import propertyDefinition._
            Some(PropertyDefinitionPropsForChapter(name, url, defaultTermName, parentType.symbol, parentComponentTypes.map(_.name), definingStatement))
          case comment: entries.Comment =>
            import comment._
            Some(CommentPropsForChapter(text, url))
          case _ =>
            Some(PlaceholderPropsForChapter(url))
        }
      }
    ChapterProps(
      chapter.title,
      getChapterUrl(bookKey, chapterKey),
      LinkSummary(book.title, getBookUrl(bookKey)),
      chapter.summary,
      entrySummaries,
      previous,
      next,
      DefinitionSummary.getAllFromContext(entryContext),
      TypeDefinitionSummary.getAllFromContext(entryContext),
      entryContext.availableEntries.ofType[DisplayShorthand],
      getDefinitionShorthands(entryContext))
  }

  @GetMapping(produces = Array("text/html;charset=UTF-8"))
  def getChapter(@PathVariable("bookKey") bookKey: String, @PathVariable("chapterKey") chapterKey: String): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- findBook(books, bookKey)
      chaptersWithKeys = getChaptersWithKeys(book)
      chapter <- findChapter(chaptersWithKeys, chapterKey)
    } yield {
      val entryContext = EntryContext.forChapterInclusive(books, book, chapter)
      createReactView("Chapter", getChapterProps(books, book, bookKey, chapter, chapterKey))
    }).toResponseEntity
  }

  @PutMapping(value = Array("/title"), produces = Array("application/json;charset=UTF-8"))
  def updateTitle(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTitle: String
  ): ResponseEntity[_] = {
    modifyChapter[Identity](bookKey, chapterKey, (_, _, _, chapter) => {
      Success(chapter.copy(title = newTitle))
    }).map{ case (books, book, chapter) => getChapterProps(books, book, bookKey, chapter, getChaptersWithKeys(book).find(_._1.title == newTitle).get._2) }.toResponseEntity
  }

  @GetMapping(value = Array("/{entryKey}"), produces = Array("text/html;charset=UTF-8"))
  def getEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- findBook(books, bookKey)
      chapter <- findChapter(book, chapterKey)
      entriesWithKeys = getEntriesWithKeys(chapter).mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[ChapterEntry.Standalone]))
      entry <- findEntry[ChapterEntry](entriesWithKeys, entryKey)
      (viewName, baseProps: Map[String, AnyRef]) <- entry match {
        case axiom: Axiom =>
          Success(("Axiom", Map("axiom" -> axiom)))
        case theorem: Theorem =>
          Success(("Theorem", Map("theorem" -> theorem, "inferences" -> getInferenceLinks(theorem.referencedInferenceIds))))
        case statementDefinition: StatementDefinition =>
          Success(("StatementDefinition", Map("definition" -> statementDefinition)))
        case termDefinition: TermDefinition =>
          Success(("TermDefinition", Map("definition" -> termDefinition)))
        case _ =>
          Failure(BadRequestException(s"Cannot view ${entry.getClass.getSimpleName}"))
      }
    } yield {
      val entryContext = EntryContext.forEntry(books, book, chapter, entry).addEntry(entry)
      val provingContext = ProvingContext(entryContext, definitions)
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
          "usages" -> getInferenceUsages(entry, books),
          "definitions" -> DefinitionSummary.getAllFromContext(entryContext),
          "typeDefinitions" -> TypeDefinitionSummary.getAllFromContext(entryContext),
          "displayShorthands" -> entryContext.availableEntries.ofType[DisplayShorthand],
          "binaryRelations" -> getBinaryRelations(provingContext),
          "definitionShorthands" -> getDefinitionShorthands(entryContext)))
    }).toResponseEntity
  }

  @PostMapping(value = Array("/theorems"), produces = Array("application/json;charset=UTF-8"))
  def createTheorem(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTheoremDefininition: NewTheoremModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      for {
        name <- Some(newTheoremDefininition.name.trim).filter(_.nonEmpty).orBadRequest("Theorem name must be given")
        premises <- newTheoremDefininition.premises.mapWithIndex((str, index) => Statement.parser.parseFromString(str, s"premise ${index + 1}").recoverWithBadRequest).traverseTry
        conclusion <- Statement.parser.parseFromString(newTheoremDefininition.conclusion, "conclusion").recoverWithBadRequest
        newTheorem = Theorem(
          name,
          premises,
          conclusion,
          Seq(Theorem.Proof(Seq(Step.Target(conclusion)))),
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

  @PostMapping(value = Array("/termDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTermDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTermDefininition: NewTermDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      val name = Option(newTermDefininition.name).filter(_.nonEmpty)
      val shorthand = Option(newTermDefininition.shorthand).filter(_.nonEmpty)
      val attributes = Option(newTermDefininition.attributes).toSeq.flatMap(_.splitByWhitespace()).filter(_.nonEmpty)
      for {
        symbol <- Option(newTermDefininition.symbol).filter(_.nonEmpty).orBadRequest("Symbol must be provided")
        boundVariablesAndComponentTypes <- ExpressionDefinition.rawBoundVariablesAndComponentTypesParser.parseFromString(newTermDefininition.components, "components").recoverWithBadRequest
        boundVariables = boundVariablesAndComponentTypes._1
        componentTypes = boundVariablesAndComponentTypes._2
        componentNames = boundVariables ++ componentTypes.map(_.name)
        definition <- Statement.parser(expressionParsingContext.addParameters("_")).parseFromString(newTermDefininition.definition, "definition").recoverWithBadRequest
        format <- Option(newTermDefininition.format).filter(_.nonEmpty).map(f => Format.parser(componentNames).parseFromString(f, "format")).getOrElse(Format.default(symbol, componentNames)).recoverWithBadRequest
        premises <- newTermDefininition.premises.mapWithIndex((str, index) => Statement.parser.parseFromString(str, s"premise ${index + 1}")).recoverWithBadRequest
        newTerm = TermDefinition(
          symbol,
          boundVariables,
          componentTypes,
          name,
          format,
          premises,
          definition,
          shorthand,
          attributes)
      } yield newTerm
    }.map{ case (books, book, chapter) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/typeDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTypeDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTypeDefininition: NewTypeDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      val name = Option(newTypeDefininition.name).filter(_.nonEmpty)
      for {
        symbol <- Option(newTypeDefininition.symbol).filter(_.nonEmpty).orBadRequest("Symbol must be provided")
        defaultTermName <- Option(newTypeDefininition.defaultTermName).filter(_.nonEmpty).orBadRequest("Default term name must be provided")
        otherComponentTypes <- ComponentType.listWithoutBoundVariablesParser.parseFromString(newTypeDefininition.otherComponents, "component types").recoverWithBadRequest
        format <- Format.parser(otherComponentTypes.map(_.name)).parseFromString(newTypeDefininition.format, "format").recoverWithBadRequest
        definition <- Statement.parser.parseFromString(newTypeDefininition.definition, "definition").recoverWithBadRequest
        newTypeDefinition = TypeDefinition(
          symbol,
          defaultTermName,
          otherComponentTypes,
          format,
          name,
          definition)
      } yield newTypeDefinition
    }.map{ case (books, book, chapter) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/propertyDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createPropertyDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newPropertyDefininition: NewPropertyDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      val name = Option(newPropertyDefininition.name).filter(_.nonEmpty)
      for {
        symbol <- Option(newPropertyDefininition.symbol).filter(_.nonEmpty).orBadRequest("Symbol must be provided")
        defaultTermName <- Option(newPropertyDefininition.defaultTermName).filter(_.nonEmpty).orBadRequest("Default term name must be provided")
        parentType <- entryContext.typeDefinitions.find(_.symbol == newPropertyDefininition.parentType).orBadRequest(s"Unknown type '${newPropertyDefininition.parentType}'")
        parentComponentTypes <- parentType.childComponentTypesParser.parseFromString(newPropertyDefininition.parentComponents, "parent component types").recoverWithBadRequest
        definition <- Statement.parser.parseFromString(newPropertyDefininition.definition, "definition").recoverWithBadRequest
        newPropertyDefinition = PropertyDefinition(
          symbol,
          parentType,
          defaultTermName,
          parentComponentTypes,
          name,
          definition)
      } yield newPropertyDefinition
    }.map{ case (books, book, chapter) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PutMapping(value = Array("/{entryKey}/index"), produces = Array("application/json;charset=UTF-8"))
  def moveEntryToIndex(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody newIndex: Int
  ): ResponseEntity[_] = {
    def tryMove(entry: ChapterEntry, previousEntries: Seq[ChapterEntry], nextEntries: Seq[ChapterEntry]): Try[Seq[ChapterEntry]] = {
      previousEntries.takeAndRemainingIfValid(newIndex).map { case (firstEntries, entriesToSkip) =>
        val inferenceIdsMoving = entry.referencedInferenceIds
        for {
          _ <- (!entriesToSkip.flatMap(_.inferences).exists(i => inferenceIdsMoving.contains(i.id))).orBadRequest("Entry depends on a previous one")
          _ <- (!entriesToSkip.exists(entry.referencedDefinitions.contains)).orBadRequest("Entry depends on a previous one")
        } yield (firstEntries :+ entry) ++ entriesToSkip ++ nextEntries
      } orElse nextEntries.takeAndRemainingIfValid(newIndex - previousEntries.length).map { case (entriesToSkip, lastEntries) =>
        val referencedInferenceIdsBeingMovedPast = entriesToSkip.flatMap(_.referencedInferenceIds)
        for {
          _ <- (!entry.inferences.map(_.id).exists(referencedInferenceIdsBeingMovedPast.contains)).orBadRequest("Entry depended on by a following one")
          _ <- (!entriesToSkip.flatMap(_.referencedDefinitions).contains(entry)).orBadRequest("Entry depended on by a following one")
        } yield (previousEntries ++ entriesToSkip :+ entry) ++ lastEntries
      } orBadRequest "Invalid index" flatten
    }
    modifyChapter[Identity](bookKey, chapterKey, (_, _, _, chapter) => {
      for {
        (previousEntries, entry, nextEntries) <- getEntriesWithKeys(chapter).splitWhere(_._2 == entryKey).orNotFound(s"Entry $entryKey")
        updatedEntries <- tryMove(entry._1, previousEntries.map(_._1), nextEntries.map(_._1))
      } yield chapter.copy(entries = updatedEntries)
    }).map{ case (books, book, chapter) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @DeleteMapping(value = Array("/{entryKey}"), produces = Array("application/json;charset=UTF-8"))
  def deleteEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ): ResponseEntity[_] = {
    def deleteEntry(chapterEntry: ChapterEntry, chapter: Chapter, books: Seq[Book]): Try[Chapter] = {
      if (getInferenceUsages(chapterEntry, books).isEmpty && getDefinitionUsages(chapterEntry, books).isEmpty)
        Success(chapter.copy(entries = chapter.entries.filter(_ != chapterEntry)))
      else
        Failure(BadRequestException("Cannot delete entry with usages"))
    }

    modifyChapter[Identity](bookKey, chapterKey, (books, _, _, chapter) =>
      for {
        entry <- findEntry[ChapterEntry](chapter, entryKey)
        updatedChapter <- deleteEntry(entry, chapter, books)
      } yield updatedChapter
    ).map{ case (books, book, chapter) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PutMapping(value = Array("/{entryKey}/shorthand"), produces = Array("application/json;charset=UTF-8"))
  def editShorthand(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newShorthand: String
  ): ResponseEntity[_] = {
    modifyEntry[ExpressionDefinition, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, definition) =>
      Success(definition.withShorthand(Option(newShorthand).filter(_.nonEmpty)))
    ).map{ case (books, book, chapter, _) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  private def getDefinitionShorthands(entryContext: EntryContext): Map[String, String] = {
    val shorthandsFromDefinitions = entryContext.availableEntries.ofType[ExpressionDefinition].mapCollect(d => d.shorthand.map(_ -> d.symbol)).toMap
    val greekLetterShorthands = 'α'.to('ω')
      .map(c => Character.getName(c).splitByWhitespace().last.toLowerCase -> c.toString)
      .toMap
    shorthandsFromDefinitions ++ greekLetterShorthands
  }

  case class BinaryRelationSummary(symbol: String, template: Statement)
  private def getBinaryRelations(provingContext: ProvingContext): Seq[BinaryRelationSummary] = {
    provingContext.definedBinaryRelations.map { case (symbol, relation) =>
      BinaryRelationSummary(symbol, relation.template)
    }
  }

  private def getInferenceUsages(entry: ChapterEntry, books: Seq[Book]): Seq[(String, String, Seq[LinkSummary])] = {
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

  private def getDefinitionUsages(entry: ChapterEntry, books: Seq[Book]): Seq[(String, String, Seq[LinkSummary])] = {
    for {
      (book, bookKey) <- getBooksWithKeys(bookService.books)
      (chapter, chapterKey) <- getChaptersWithKeys(book)
      entriesWithKeys = getEntriesWithKeys(chapter)
        .filter(_._1.referencedDefinitions.contains(entry))
      if entriesWithKeys.nonEmpty
    } yield (book.title, chapter.title, entriesWithKeys.map { case (entry, key) => LinkSummary(entry.name, getEntryUrl(bookKey, chapterKey, key))})
  }
}

object ChapterController {
  case class NewTheoremModel(
    name: String,
    premises: Seq[String],
    conclusion: String)
  case class NewTermDefinitionModel(
    symbol: String,
    components: String,
    name: String,
    format: String,
    premises: Seq[String],
    definition: String,
    shorthand: String,
    attributes: String)
  case class NewTypeDefinitionModel(
    symbol: String,
    defaultTermName: String,
    otherComponents: String,
    format: String,
    name: String,
    definition: String)
  case class NewPropertyDefinitionModel(
    symbol: String,
    parentType: String,
    defaultTermName: String,
    parentComponents: String,
    name: String,
    definition: String)
}
