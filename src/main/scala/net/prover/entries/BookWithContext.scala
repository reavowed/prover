package net.prover.entries

import net.prover.books.keys.GetWithKeys
import net.prover.books.model.Book
import net.prover.controllers.{BookService, OptionWithResponseExceptionOps}
import net.prover.model.Chapter
import net.prover.model.definitions.Definitions

import scala.util.Try

case class BookWithContext(
    book: Book,
    bookKey: String,
    globalContext: GlobalContext
) {
  def allBooks: List[Book] = globalContext.allBooks
  def definitions: Definitions = globalContext.definitions

  lazy val chaptersWithKeys: List[(Chapter, String)] = GetWithKeys(book.chapters)
  def chaptersWithContexts: Seq[ChapterWithContext] = book.chapters.map(getChapter)

  def getChapter(chapter: Chapter): ChapterWithContext = {
    chaptersWithKeys.find(_._1 == chapter).map(getChapter).get
  }
  def getChapter(chapterKey: String): Try[ChapterWithContext] = {
    chaptersWithKeys.find(_._2 == chapterKey).map(getChapter).orNotFound(s"Chapter $chapterKey")
  }
  private def getChapter(tuple: (Chapter, String)): ChapterWithContext = {
    ChapterWithContext(tuple._1, tuple._2, this)
  }
}
