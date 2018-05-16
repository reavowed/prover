package net.prover.model

import net.prover.model.entries._

case class Chapter(
    title: String,
    key: Chapter.Key,
    summary: String,
    entries: Seq[ChapterEntry])
{
  def statementDefinitions = entries.ofType[StatementDefinition]
  def termDefinitions = entries.ofType[TermDefinition]
  def inferences = entries.flatMap(_.inferences)
  def theorems = entries.ofType[Theorem]
  def shorthands = entries.ofType[Shorthand]

  def serialized: String = {
    val entryTexts = entries.map(_.serializedLines.mkString("\n"))
    (summary +: entryTexts).mkString("\n\n") + "\n"
  }
}

object Chapter {

  case class Key(value: String, bookKey: Book.Key) {
    def url = s"${bookKey.url}/$value"
  }

  val chapterEntryParsers: Seq[ChapterEntryParser] = Seq(
    Comment,
    StatementDefinition,
    TermDefinition,
    Axiom,
    Theorem,
    Shorthand)

  def chapterEntryParser(getKey: String => ChapterEntry.Key)(context: ParsingContext): Parser[Option[ChapterEntry]] = {
    Parser.singleWordIfAny.flatMapFlatMapReverse { entryType =>
      chapterEntryParsers.find(_.name == entryType).map(_.parser(getKey)(context))
    }
  }

  def parser(title: String, bookKey: Book.Key)(initialContext: ParsingContext): Parser[(Chapter, ParsingContext)] = {
    val key = Key(title.formatAsKey, bookKey)
    for {
      summary <- Parser.toEndOfLine
      entriesAndContext <- Parser.foldWhileDefined[ChapterEntry, ParsingContext](initialContext) { (entriesSoFar, currentContext) =>
        def getNextKey(entryName: String): ChapterEntry.Key = {
          val entryKeyValue = entriesSoFar.ofType[ChapterEntry.WithKey].count(_.name == entryName) match {
            case 0 =>
              entryName.formatAsKey
            case n =>
              (entryName + " " + (n+1)).formatAsKey
          }
          ChapterEntry.Key(entryKeyValue, key)
        }
        chapterEntryParser(getNextKey)(currentContext).mapMap { entry =>
          (entry, currentContext.add(entry))
        }
      }
    } yield Chapter(title, key, summary, entriesAndContext._1) -> entriesAndContext._2
  }
}
