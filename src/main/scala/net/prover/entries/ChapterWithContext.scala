package net.prover.entries

import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.{Chapter, Inference}

import scala.reflect.{ClassTag, classTag}
import scala.util.Try

case class ChapterWithContext(
    chapter: Chapter,
    chapterKey: String,
    bookWithContext: BookWithContext
) {
  def globalContext: GlobalContext = bookWithContext.globalContext

  def entriesWithContexts: Seq[EntryWithContext] = chapter.entriesWithKeys.listWithKeys.map(getEntry[ChapterEntry])
  def inferencesWithContexts: Seq[TypedEntryWithContext[Inference.Entry]] = entriesWithContexts.ofType[TypedEntryWithContext[Inference.Entry]]
  def theoremsWithContexts: Seq[TheoremWithContext] = entriesWithContexts.filter(_.entry.isInstanceOf[Theorem]).map(_.asInstanceOf[TheoremWithContext])

  def getEntry[T <: ChapterEntry : ClassTag](entryKey: String): Try[TypedEntryWithContext[T]] = {
    chapter.entriesWithKeys.listWithKeys.find(_._2 == entryKey)
      .orNotFound(s"Chapter $chapterKey")
      .flatMap(_._1.asOptionalInstanceOf[T].orBadRequest(s"Entry is not a ${classTag[T].runtimeClass.getSimpleName}"))
      .map(TypedEntryWithContext(_, entryKey, this))
  }

  def getEntry[T <: ChapterEntry : ClassTag](entry: T): TypedEntryWithContext[T] = {
    val key = chapter.entriesWithKeys.listWithKeys.find(_._1 == entry).map(_._2).get
    TypedEntryWithContext(entry, key, this)
  }

  private def getEntry[T <: ChapterEntry : ClassTag](tuple: (T, String)): TypedEntryWithContext[T] = {
    TypedEntryWithContext(tuple._1, tuple._2, this)
  }
}
