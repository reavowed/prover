package net.prover.entries

import net.prover.model.entries.ChapterEntry
import net.prover.model.{Book, Chapter, EntryContext}

case class EntryWithContext(book: Book, chapter: Chapter, entry: ChapterEntry, entryContext: EntryContext)
