package net.prover.model

import net.prover.model.entries._

case class Chapter(
    title: String,
    summary: String,
    entries: Seq[ChapterEntry])
{
  def serialized: String = {
    val entryTexts = entries.map(_.serializedLines.mkString("\n"))
    (summary +: entryTexts).mkString("\n\n") + "\n"
  }

  def addEntry(newEntry: ChapterEntry): Chapter = copy(entries = entries :+ newEntry)
}

object Chapter {
  val chapterEntryParsers: Seq[ChapterEntryParser] = Seq(
    Comment,
    StatementDefinition,
    TermDefinition,
    Axiom,
    Theorem,
    DisplayShorthand,
    WritingShorthand)

  def chapterEntryParser(context: EntryContext): Parser[Option[ChapterEntry]] = {
    Parser.singleWordIfAny.flatMapFlatMapReverse { entryType =>
      chapterEntryParsers.find(_.name == entryType).map(_.parser(context))
    }
  }

  def parser(title: String)(initialContext: EntryContext): Parser[(Chapter, EntryContext)] = {
    for {
      summary <- Parser.toEndOfLine
      entriesAndContext <- Parser.foldWhileDefined[ChapterEntry, EntryContext](initialContext) { (_, _, currentContext) =>
        chapterEntryParser(currentContext).mapMap { entry =>
          (entry, currentContext.addEntry(entry))
        }
      }
    } yield Chapter(title, summary, entriesAndContext._1) -> entriesAndContext._2
  }
}
