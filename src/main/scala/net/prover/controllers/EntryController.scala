package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model.entries.{ChapterEntry, ExpressionDefinition, TypeDefinition}
import net.prover.model._
import net.prover.model.definitions.Definitions
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{entryKey}"))
class EntryController @Autowired() (val bookService: BookService) extends BookModification {

  @PutMapping(value = Array("/name"), produces = Array("application/json;charset=UTF-8"))
  def editName(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newName: String
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ChapterEntry, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, entry) => entry match {
      case inference: Inference.Entry =>
        Success(inference.withName(newName))
      case definition: ExpressionDefinition =>
        Success(definition.withName(Option(newName).filter(_.nonEmpty)))
      case _ =>
        Failure(BadRequestException(s"Cannot set name of ${entry.getClass.getName}"))
    })
      .flatMap { case (_, _, _, chapter, entry) =>
        for {
          (_, newKey) <- BookService.getEntriesWithKeys(chapter).find(_._1 == entry).orException(new Exception("Couldn't find new entry"))
        } yield BookService.getEntryUrl(bookKey, chapterKey, newKey)
      }
      .toResponseEntity
  }

  @PutMapping(value = Array("/symbol"), produces = Array("application/json;charset=UTF-8"))
  def editSymbol(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newSymbol: String
  ): ResponseEntity[_] = {
    (for {
      book <- bookService.findBook(bookKey)
      chapter <- bookService.findChapter(book, chapterKey)
      entry <- bookService.findEntry[ChapterEntry](chapter, entryKey)
      (newEntry, newBooks) <- entry match {
        case definition: ExpressionDefinition =>
          val newDefinition = definition.withSymbol(newSymbol)
          val newDefinitionWithFormat = if (definition.format.isInstanceOf[Format.Default]) newDefinition.withFormat(Format.default(newSymbol, definition.boundVariableNames ++ definition.componentTypes.map(_.name))) else newDefinition
          Success((newDefinition, modifyExpressionDefinition(definition, newDefinitionWithFormat)._1))
        case definition: TypeDefinition =>
          val newDefinition = definition.withSymbol(newSymbol)
          Success((newDefinition, modifyTypeDefinition(definition, newDefinition)._1))
        case _ =>
          Failure(BadRequestException(s"Cannot edit symbol of ${entry.getClass.getName}"))
      }
      newBook <- bookService.findBook(newBooks, bookKey)
      newChapter <- bookService.findChapter(newBook, chapterKey)
      newKey <- BookService.getEntriesWithKeys(newChapter).find(_._1 == newEntry).map(_._2).orException(new Exception("Couldn't find new entry"))
    } yield BookService.getEntryUrl(bookKey, chapterKey, newKey)).toResponseEntity
  }

  @PutMapping(value = Array("/attributes"), produces = Array("application/json;charset=UTF-8"))
  def editAttributes(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newAttributes: Seq[String]
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ChapterEntry, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, entry) => entry match {
      case definition: ExpressionDefinition =>
        Success(definition.withAttributes(newAttributes))
      case _ =>
        Failure(BadRequestException(s"Cannot set attributes of ${entry.getClass.getName}"))
    }).toResponseEntity
  }

  @PutMapping(value = Array("/format"), produces = Array("application/json;charset=UTF-8"))
  def editFormat(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newFormatText: String
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ChapterEntry, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, entry) => {
      entry match {
        case definition: ExpressionDefinition =>
          val componentNames = definition.boundVariableNames ++ definition.componentTypes.map(_.name)
          for {
            format <- Format.parser(componentNames).parseFromString(newFormatText, "format").recoverWithBadRequest
          } yield definition.withFormat(format)
        case definition: TypeDefinition =>
          val componentNames = definition.otherComponentTypes.map(_.name)
          for {
            format <- Format.parser(componentNames).parseFromString(newFormatText, "format").recoverWithBadRequest
          } yield definition.withFormat(format)

        case _ =>
          Failure(BadRequestException(s"Cannot set format of ${entry.getClass.getName}"))
      }
    }).toResponseEntity
  }

  private def modifyDefinitions(
    books: Seq[Book],
    entriesToReplace: Map[ChapterEntry, ChapterEntry],
    definitionsToReplace: Map[ExpressionDefinition, ExpressionDefinition]
  ): Seq[Book] = {
    books.mapReduceWithPrevious[Book] { (previousBooks, bookToModify) =>
      bookToModify.chapters.mapFold(EntryContext.forBookExclusive(previousBooks, bookToModify)) { (entryContextForChapter, chapterToModify) =>
        chapterToModify.entries.mapFold(entryContextForChapter) { (entryContext, entryToModify) =>
          val modifiedEntry = entriesToReplace.getOrElse(entryToModify, definitionsToReplace.foldLeft(entryToModify) { case (entry, (oldDefinition, newDefinition)) =>
            entry.replaceDefinition(oldDefinition, newDefinition, entryContext)
          })
          (entryContext.addEntry(modifiedEntry), modifiedEntry)
        }.mapRight(newEntries => chapterToModify.copy(entries = newEntries))
      }.mapRight(newChapters => bookToModify.copy(chapters = newChapters))._2
    }
  }

  private def modifyExpressionDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): (Seq[Book], Definitions) = {
    bookService.modifyBooks[Identity]((books, _) => {
      modifyDefinitions(books, Map(oldDefinition -> newDefinition), Map(oldDefinition -> newDefinition))
    })
  }

  private def modifyTypeDefinition(oldDefinition: TypeDefinition, newDefinition: TypeDefinition): (Seq[Book], Definitions) = {
    bookService.modifyBooks[Identity]((books, definitions) => {
      val propertyDefinitions = definitions.rootEntryContext.propertyDefinitionsByType(oldDefinition.symbol)
      modifyDefinitions(
        books,
        ((oldDefinition -> newDefinition) +: propertyDefinitions.map(p => p -> p.copy(parentType = newDefinition))).toMap,
        ((oldDefinition.statementDefinition -> newDefinition.statementDefinition) +: propertyDefinitions.map(p => p.statementDefinition -> p.copy(parentType = newDefinition).statementDefinition)).toMap)
    })
  }
}
