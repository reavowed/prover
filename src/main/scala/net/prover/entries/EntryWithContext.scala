package net.prover.entries

import net.prover.books.model.Book
import net.prover.model.entries.ChapterEntry
import net.prover.model.{Chapter, EntryContext}

case class EntryWithContext(book: Book, chapter: Chapter, entry: ChapterEntry, entryContext: EntryContext)
