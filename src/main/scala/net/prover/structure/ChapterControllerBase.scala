package net.prover.structure

import net.prover._
import net.prover.controllers.{BookModification, ParameterValidation}
import net.prover.model.definitions.Definitions
import net.prover.structure.datatransfer.ChapterProps.EntryProps
import net.prover.structure.datatransfer.{ChapterProps, LinkSummary}
import net.prover.structure.model._
import net.prover.structure.model.entries.{Axiom, ChapterEntry, Comment, CompoundStatementDefinitionEntry, CompoundTermDefinitionEntry, Theorem}
import net.prover.types.model.entries.{PropertyDefinitionOnType, RelatedObjectDefinition, StandalonePropertyDefinition, TypeDefinition, TypeQualifierDefinition, TypeRelationDefinition}

import scala.util.Try

trait ChapterControllerBase extends BookModification with ParameterValidation with ReactViews {

  def getChapterProps(books: Seq[Book], definitions: Definitions, book: Book, bookKey: String, chapter: Chapter, chapterKey: String): Map[String, AnyRef] = {
    val chaptersWithKeys = BookService.getChaptersWithKeys(book)
    val index = chaptersWithKeys.findIndexWhere(_._1 == chapter).getOrElse(throw new Exception("Book somehow didn't exist"))
    val previous = chaptersWithKeys.lift(index - 1).map { case (c, key) => LinkSummary(c.title, BookService.getChapterUrl(bookKey, key)) }
    val next = chaptersWithKeys.lift(index + 1).map { case (c, key) => LinkSummary(c.title, BookService.getChapterUrl(bookKey, key)) }
    implicit val entryContext = EntryContext.forChapterInclusive(books, book, chapter)

    val entrySummaries = BookService.getEntriesWithKeys(chapter)
      .map(_.mapRight(key => BookService.getEntryUrl(bookKey, chapterKey, key)))
      .mapCollect { case (entry, url) =>
        entry match {
          case axiom: Axiom =>
            Some(EntryProps("axiom", url, axiom.title, ChapterProps.InferenceSummaryForChapter(axiom, definitions)))
          case theorem: Theorem =>
            Some(EntryProps("theorem", url, theorem.title, ChapterProps.InferenceSummaryForChapter(theorem, definitions)))
          case statementDefinition: CompoundStatementDefinitionEntry =>
            Some(EntryProps("statementDefinition", url, statementDefinition.title, statementDefinition))
          case termDefinition: CompoundTermDefinitionEntry =>
            Some(EntryProps("statementDefinition", url, termDefinition.title, termDefinition))
          case typeDefinition: TypeDefinition =>
            Some(EntryProps("typeDefinition", url, typeDefinition.title, typeDefinition))
          case typeQualifierDefinition: TypeQualifierDefinition =>
            Some(EntryProps("typeQualifierDefinition", url, typeQualifierDefinition.title, typeQualifierDefinition))
          case propertyDefinition: PropertyDefinitionOnType =>
            Some(EntryProps("propertyDefinition", url, propertyDefinition.title, propertyDefinition))
          case relatedObjectDefinition: RelatedObjectDefinition =>
            Some(EntryProps("relatedObjectDefinition", url, relatedObjectDefinition.title, relatedObjectDefinition))
          case typeRelationDefinition: TypeRelationDefinition =>
            Some(EntryProps("typeRelationDefinition", url, typeRelationDefinition.title, typeRelationDefinition))
          case standalonePropertyDefinition: StandalonePropertyDefinition =>
            Some(EntryProps("standalonePropertyDefinition", url, standalonePropertyDefinition.title, standalonePropertyDefinition))
          case comment: Comment =>
            Some(EntryProps("comment", url, None, comment.text))
          case _ =>
            Some(EntryProps("placeholder", url, None, None))
        }
      }
    Map(
      "title" -> chapter.title,
      "url" -> BookService.getChapterUrl(bookKey, chapterKey),
      "bookLink" -> LinkSummary(book.title, BookService.getBookUrl(bookKey)),
      "summary" -> chapter.summary,
      "entries" -> entrySummaries,
      "previous" -> previous,
      "next" -> next
    ) ++ getGeneralDisplayProps(entryContext)
  }

  def addChapterEntry(bookKey: String, chapterKey: String)(f: (Seq[Book], Book, Chapter) => Try[ChapterEntry]): Try[(Seq[Book], Definitions, Book, Chapter)] = {
    bookService.modifyChapter[Identity](bookKey, chapterKey, (books, _, book, chapter) =>
      for {
        entry <- f(books, book, chapter)
        _ <- entry.validate().recoverWithBadRequest
      } yield chapter.addEntry(entry)
    )
  }
}
