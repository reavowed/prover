package net.prover.model

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.entries._

case class Chapter(
    title: String,
    key: Chapter.Key,
    summary: String,
    entries: Seq[ChapterEntry])
{
  def statementDefinitions = entries.ofType[StatementDefinition]
  def termDefinitions = entries.ofType[TermDefinition]
  def definitions = entries.ofType[ExpressionDefinition]
  def inferences = entries.flatMap(_.inferences)
  def theorems = entries.ofType[Theorem]
  def displayShorthands = entries.ofType[DisplayShorthand]

  def serialized: String = {
    val entryTexts = entries.map(_.serializedLines.mkString("\n"))
    (summary +: entryTexts).mkString("\n\n") + "\n"
  }

  def addEntry(newEntry: ChapterEntry): Chapter = copy(entries = entries :+ newEntry)
}

object Chapter {

  case class Key(name: String, value: String, bookKey: Book.Key) {
    @JsonSerialize
    def url = s"${bookKey.url}/$value"
  }

  val chapterEntryParsers: Seq[ChapterEntryParser] = Seq(
    Comment,
    StatementDefinition,
    TermDefinition,
    Axiom,
    Theorem,
    DisplayShorthand)

  def getNextKey(existingEntries: Seq[ChapterEntry], entryName: String) = {
    existingEntries.ofType[ChapterEntry.WithKey].count(_.name == entryName) match {
      case 0 =>
        entryName.formatAsKey
      case n =>
        (entryName + " " + (n+1)).formatAsKey
    }
  }

  def chapterEntryParser(getKey: String => (String, Chapter.Key))(context: ParsingContext): Parser[Option[ChapterEntry]] = {
    Parser.singleWordIfAny.flatMapFlatMapReverse { entryType =>
      chapterEntryParsers.find(_.name == entryType).map(_.parser(getKey)(context))
    }
  }

  def parser(title: String, bookKey: Book.Key)(initialContext: ParsingContext): Parser[(Chapter, ParsingContext)] = {
    val key = Key(title, title.formatAsKey, bookKey)
    for {
      summary <- Parser.toEndOfLine
      entriesAndContext <- Parser.foldWhileDefined[ChapterEntry, ParsingContext](initialContext) { (entriesSoFar, currentContext) =>
        chapterEntryParser(x => (getNextKey(entriesSoFar, x), key))(currentContext).mapMap { entry =>
          (entry, currentContext.add(entry))
        }
      }
    } yield Chapter(title, key, summary, entriesAndContext._1) -> entriesAndContext._2
  }
}
