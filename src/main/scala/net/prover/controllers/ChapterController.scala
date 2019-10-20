package net.prover.controllers

import net.prover.controllers.ChapterController._
import net.prover.controllers.models.ChapterProps._
import net.prover.controllers.models.{ChapterProps, LinkSummary}
import net.prover.exceptions.BadRequestException
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries._
import net.prover.model.expressions.{DefinedStatement, FunctionParameter, Statement, Template, TermVariable}
import net.prover.model.proof.{InferenceTypes, Step}
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

    val entrySummaries = getEntriesWithKeys(chapter).mapCollect{
      case (axiom: entries.Axiom, key) =>
        import axiom._
        Some(AxiomPropsForChapter(name, getEntryUrl(bookKey, chapterKey, key), premises, conclusion))
      case (theorem: entries.Theorem, key) =>
        import theorem._
        Some(TheoremPropsForChapter(name, getEntryUrl(bookKey, chapterKey, key), premises, conclusion, isComplete))
      case (statementDefinition: entries.StatementDefinition, key) =>
        import statementDefinition._
        Some(StatementDefinitionPropsForChapter(defaultValue, getEntryUrl(bookKey, chapterKey, key), shorthand, definingStatement))
      case (termDefinition: entries.TermDefinition, key) =>
        import termDefinition._
        Some(TermDefinitionPropsForChapter(defaultValue, getEntryUrl(bookKey, chapterKey, key), shorthand, definingStatement, premises))
      case (typeDefinition: entries.TypeDefinition, key) =>
        import typeDefinition._
        Some(TypeDefinitionPropsForChapter(symbol, getEntryUrl(bookKey, chapterKey, key), defaultTermName, otherComponentTypes.map(_.name), definingStatement))
      case (propertyDefinition: entries.PropertyDefinition, key) =>
        import propertyDefinition._
        Some(PropertyDefinitionPropsForChapter(name, getEntryUrl(bookKey, chapterKey, key), defaultTermName, parentType.symbol, parentComponentTypes.map(_.name), definingStatement))
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

  @GetMapping(produces = Array("text/html;charset=UTF-8"))
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
          "typeDefinitions" -> getTypeDefinitions(entryContext),
          "displayShorthands" -> entryContext.availableEntries.ofType[DisplayShorthand],
          "definitionShorthands" -> getDefinitionShorthands(entryContext)))
    }).toResponseEntity
  }

  @PutMapping(value = Array("/title"), produces = Array("application/json;charset=UTF-8"))
  def updateTitle(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTitle: String
  ): ResponseEntity[_] = {
    modifyChapter[Identity](bookKey, chapterKey, (_, _, chapter) => {
      Success(chapter.copy(title = newTitle))
    }).map{ case (books, book, chapter) => getChapterProps(books, book, bookKey, chapter, getChaptersWithKeys(book).find(_._1.title == newTitle).get._2) }.toResponseEntity
  }

  case class InferenceProps(inference: Inference.Entry, previousEntry: Option[LinkSummary], nextEntry: Option[LinkSummary], usages: Seq[(Book, Chapter, Seq[Theorem])])
  @GetMapping(value = Array("/{entryKey}"), produces = Array("text/html;charset=UTF-8"))
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
          "typeDefinitions" -> getTypeDefinitions(entryContext),
          "displayShorthands" -> entryContext.availableEntries.ofType[DisplayShorthand],
          "transitiveStatements" -> getTransitiveStatements(entryContext),
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
        premises <- newTheoremDefininition.premises.mapWithIndex((str, index) => Statement.parser.parseFromString(str, s"premise ${index + 1}").recoverWithBadRequest).traverseTry
        conclusion <- Statement.parser.parseFromString(newTheoremDefininition.conclusion, "conclusion").recoverWithBadRequest
        newTheorem = Theorem(
          newTheoremDefininition.name,
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
      val symbol = newTermDefininition.symbol
      val name = Option(newTermDefininition.name).filter(_.nonEmpty)
      val shorthand = Option(newTermDefininition.shorthand).filter(_.nonEmpty)
      val attributes = Option(newTermDefininition.attributes).toSeq.flatMap(_.splitByWhitespace()).filter(_.nonEmpty)
      for {
        boundVariablesAndComponentTypes <- ExpressionDefinition.rawBoundVariablesAndComponentTypesParser.parseFromString(newTermDefininition.components, "components").recoverWithBadRequest
        boundVariables = boundVariablesAndComponentTypes._1
        componentTypes = boundVariablesAndComponentTypes._2
        componentNames = boundVariables ++ componentTypes.map(_.name)
        definition <- Statement.parser.parseFromString(newTermDefininition.definition, "definition").recoverWithBadRequest
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
      val symbol = newTypeDefininition.symbol
      val defaultTermName = newTypeDefininition.defaultTermName
      val name = Option(newTypeDefininition.name).filter(_.nonEmpty)
      for {
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
      val symbol = newPropertyDefininition.symbol
      val defaultTermName = newPropertyDefininition.defaultTermName
      val name = Option(newPropertyDefininition.name).filter(_.nonEmpty)
      for {
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


  @PostMapping(value = Array("/{entryKey}/move"), produces = Array("application/json;charset=UTF-8"))
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
            _ <- Some(precedingEntry).filter(entry.referencedEntries.contains).badRequestIfDefined("Entry depends on previous one")
          } yield earlierEntries ++ Seq(entry, precedingEntry) ++ nextEntries
        case "down" =>
          for {
            (nextEntry, lastEntries) <- +:.unapply(nextEntries).orBadRequest("Entry was last in chapter")
            _ <- entry.asOptionalInstanceOf[Inference].filter(i => nextEntry.referencedInferenceIds.contains(i.id)).badRequestIfDefined("Next entry depends on this one")
            _ <- Some(entry).filter(nextEntry.referencedEntries.contains).badRequestIfDefined("Next entry depends on this one")
          } yield previousEntries ++ Seq(nextEntry, entry) ++ lastEntries
      }
    }
    modifyChapter[Identity](bookKey, chapterKey, (_, _, chapter) => {
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
      chapterEntry match {
        case inference: Inference if getUsages(inference, books).isEmpty =>
          Success(chapter.copy(entries = chapter.entries.filter(_ != inference)))
        case _: Inference =>
          Failure(BadRequestException("Cannot delete inference with usages"))
        case _ =>
          Failure(BadRequestException("Deleting non-inference entries not yet supported"))
      }
    }

    modifyChapter[Identity](bookKey, chapterKey, (books, _, chapter) =>
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
    modifyEntry[ExpressionDefinition, Identity](bookKey, chapterKey, entryKey, (_, _, _, definition) =>
      Success(definition.withShorthand(Option(newShorthand).filter(_.nonEmpty)))
    ).map{ case (books, book, chapter, _) => getChapterProps(books, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PutMapping(value = Array("/{entryKey}/symbol"), produces = Array("application/json;charset=UTF-8"))
  def editSymbol(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newSymbol: String
  ): ResponseEntity[_] = {
    (for {
      book <- findBook(bookKey)
      chapter <- findChapter(book, chapterKey)
      entry <- findEntry[ExpressionDefinition](chapter, entryKey)
      newEntry = entry.withSymbol(newSymbol)
      newBooks = bookService.modifyBooks[Identity](books => {
        books.mapReduceWithPrevious[Book] { (previousBooks, bookToModify) =>
          bookToModify.chapters.mapFold(EntryContext.forBookExclusive(previousBooks, bookToModify)) { (entryContextForChapter, chapterToModify) =>
            chapterToModify.entries.mapFold(entryContextForChapter) { (entryContext, entryToModify) =>
              val modifiedEntry = if (entryToModify == entry) {
                newEntry
              } else {
                entryToModify.replaceDefinition(entry, newEntry, entryContext)
              }
              (entryContext.addEntry(modifiedEntry), modifiedEntry)
            }.mapRight(newEntries => chapterToModify.copy(entries = newEntries))
          }.mapRight(newChapters => bookToModify.copy(chapters = newChapters))._2
        }
      })
    } yield ()).toResponseEntity
  }

  case class DefinitionSummary(symbol: String, baseFormatString: String, requiresBrackets: Boolean, requiresComponentBrackets: Boolean, numberOfBoundVariables: Int, attributes: Seq[String])
  private def getDefinitionSummaries(entryContext: EntryContext) = {
    entryContext.availableEntries.ofType[ExpressionDefinition]
      .map(d => d.symbol -> DefinitionSummary(d.symbol, d.format.baseFormatString, d.format.requiresBrackets, d.format.requiresComponentBrackets, d.boundVariableNames.length, d.attributes))
      .toMap
  }

  case class TypeDefinitionSummary(symbol: String, name: String, componentFormatString: String, article: String, properties: Map[String, String])
  private def getTypeDefinitions(entryContext: EntryContext) = {
    entryContext.typeDefinitions
      .map(d => d.symbol -> TypeDefinitionSummary(
        d.symbol,
        d.name,
        d.componentFormat.baseFormatString,
        d.article,
        entryContext.propertyDefinitionsByType.getOrElse(d.symbol, Nil).map(pd => pd.qualifiedSymbol -> pd.name).toMap))
      .toMap
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

  case class TransitivityStatement(symbol: String, template: Statement, inferenceId: String)
  private def getTransitiveStatements(entryContext: EntryContext) = {
    getTransitiveStatementsFromDefinitions(entryContext) ++ getTransitiveStatementsFromShorthands(entryContext)
  }

  // Find all statement definitions that have default infix formats and a proven transitivity
  private def getTransitiveStatementsFromDefinitions(entryContext: EntryContext): Seq[TransitivityStatement] = {
    for {
      definition <- entryContext.statementDefinitions
      if definition.componentTypes.length == 2 && definition.format.baseFormatString == s"%0 ${definition.symbol} %1"
      predicate = DefinedStatement(Seq(FunctionParameter(0, 0), FunctionParameter(1, 0)), definition)(Nil)
      inference <- entryContext.inferences.find { i => InferenceTypes.getTransitivityPredicate(i).contains(predicate) }.toSeq
    } yield TransitivityStatement(definition.symbol, definition.defaultValue, inference.id)
  }

  // Find all term definitions that can be built into an infix statement via a shorthand and have a proven transitivity
  private def getTransitiveStatementsFromShorthands(entryContext: EntryContext): Seq[TransitivityStatement] = {
    val shorthands = entryContext.availableEntries.ofType[DisplayShorthand]
    for {
      shorthand <- entryContext.availableEntries.ofType[DisplayShorthand]
      if shorthand.template.isInstanceOf[Template.DefinedStatement]
      if shorthand.template.variables.length == 3
      Seq(lhsIndex, symbolIndex, rhsIndex) <- "%(\\d) %(\\d) %(\\d)".r.unapplySeq(shorthand.format.baseFormatString).map(_.map(_.toInt)).toSeq
      if lhsIndex != symbolIndex && lhsIndex != rhsIndex && symbolIndex != rhsIndex
      lhsVariable = shorthand.template.variables(lhsIndex)
      symbolVariable = shorthand.template.variables(symbolIndex)
      rhsVariable = shorthand.template.variables(rhsIndex)
      if lhsVariable.isInstanceOf[Template.TermVariable] && symbolVariable.isInstanceOf[Template.TermVariable] && rhsVariable.isInstanceOf[Template.TermVariable]
      if shorthand.conditions.forall(_._1 == symbolVariable.name)
      definition <- entryContext.termDefinitions
      if definition.componentTypes.isEmpty
      if shorthand.conditions.map(_._2).forall(definition.attributes.contains)
      predicate = shorthand.template.expand(Map.empty, Map(lhsVariable.name -> FunctionParameter(0, 0), rhsVariable.name -> FunctionParameter(1, 0), symbolVariable.name -> definition.defaultValue))
      inference <- entryContext.inferences.find { i => InferenceTypes.getTransitivityPredicate(i).contains(predicate) }.toSeq
    } yield TransitivityStatement(
      definition.symbol,
      shorthand.template.expand(Map.empty, Map(lhsVariable.name -> TermVariable(lhsVariable.name), rhsVariable.name -> TermVariable(rhsVariable.name), symbolVariable.name -> definition.defaultValue)).asInstanceOf[Statement],
      inference.id)
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
