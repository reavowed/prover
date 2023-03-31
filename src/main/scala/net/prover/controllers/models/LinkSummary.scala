package net.prover.controllers.models

import net.prover.controllers.BookService
import net.prover.entries.{BookWithContext, ChapterWithContext, EntryWithContext}

case class LinkSummary(title: String, url: String)

object LinkSummary {
  def apply(bookWithContext: BookWithContext): LinkSummary = LinkSummary(bookWithContext.book.title, BookService.getBookUrl(bookWithContext))
  def apply(chapterWithContext: ChapterWithContext): LinkSummary = LinkSummary(chapterWithContext.chapter.title, BookService.getChapterUrl(chapterWithContext))
  def apply(entryWithContext: EntryWithContext): LinkSummary = LinkSummary(entryWithContext.entry.name, BookService.getEntryUrl(entryWithContext))
}
