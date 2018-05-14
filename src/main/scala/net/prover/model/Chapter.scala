package net.prover.model

import net.prover.model.entries._

case class Chapter(
    title: String,
    summary: String,
    bookTitle: String,
    entries: Seq[ChapterEntry] = Nil)
{
  val key: String = title.formatAsKey
  val bookKey: String = bookTitle.formatAsKey

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

  val chapterEntryParsers: Seq[ChapterEntryParser] = Seq(
    Comment,
    StatementDefinition,
    TermDefinition,
    Axiom,
    Theorem,
    Shorthand)

  def chapterEntryParser(chapterTitle: String, bookTitle: String, getKey: String => String)(context: ParsingContext): Parser[Option[ChapterEntry]] = {
    Parser.singleWordIfAny.flatMapFlatMapReverse { key =>
      chapterEntryParsers.find(_.name == key).map(_.parser(chapterTitle, bookTitle, getKey)(context))
    }
  }

  def parser(title: String, bookTitle: String)(initialContext: ParsingContext): Parser[(Chapter, ParsingContext)] = {
    for {
      summary <- Parser.toEndOfLine
      entriesAndContext <- Parser.foldWhileDefined[ChapterEntry, ParsingContext](initialContext) { (entriesSoFar, currentContext) =>
        def getNextKey(name: String): String = {
          entriesSoFar.flatMap(_.inferences).count(_.name == name) match {
            case 0 =>
              name.formatAsKey
            case n =>
              (name + " " + (n+1)).formatAsKey
          }
        }
        chapterEntryParser(title, bookTitle, getNextKey)(currentContext).mapMap { entry =>
          (entry, currentContext.add(entry))
        }
      }
    } yield Chapter(title, summary, bookTitle, entriesAndContext._1) -> entriesAndContext._2
  }
}
