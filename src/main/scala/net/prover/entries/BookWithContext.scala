package net.prover.entries

import net.prover.books.model.Book
import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.model.Chapter

import scala.util.Try

case class BookWithContext(
    book: Book,
    bookKey: String,
    globalContext: GlobalContext
) {
  def chaptersWithContexts: Seq[ChapterWithContext] = book.chaptersWithKeys.listWithKeys.map(getChapter)

  def getChapter(chapter: Chapter): ChapterWithContext = {
    book.chaptersWithKeys.listWithKeys.find(_._1 == chapter).map(getChapter).get
  }
  def getChapter(chapterKey: String): Try[ChapterWithContext] = {
    book.chaptersWithKeys.listWithKeys.find(_._2 == chapterKey).map(getChapter).orNotFound(s"Chapter $chapterKey")
  }
  private def getChapter(tuple: (Chapter, String)): ChapterWithContext = {
    ChapterWithContext(tuple._1, tuple._2, this)
  }
}
