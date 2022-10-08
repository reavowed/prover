package net.prover.model

import net.prover.model.entries.ChapterEntry

case class EntryParsingContext(entryContext: EntryContext) {
  def addEntry(entry: ChapterEntry): EntryParsingContext = copy(entryContext = entryContext.addEntry(entry))
}
