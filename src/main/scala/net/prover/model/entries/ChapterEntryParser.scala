package net.prover.model.entries

import net.prover.entries.EntryParsingContext
import net.prover.parsing.{KnownWordParser, Parser}

trait ChapterEntryParser {
  def name: String
  def parser(implicit entryParsingContext: EntryParsingContext): Parser[ChapterEntry]

  def knownWordParser(entryParsingContext: EntryParsingContext): KnownWordParser[ChapterEntry] = {
    KnownWordParser(name)(parser(entryParsingContext))
  }
}
