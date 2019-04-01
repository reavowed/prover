package net.prover.controllers.models

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.{Book, Chapter, entries}
import net.prover.model.entries.ChapterEntry
import net.prover.model.expressions.{Statement, Term}

case class ChapterProps(
  title: String,
  chapterKey: Chapter.Key,
  summary: String,
  entries: Seq[ChapterProps.Entry],
  previous: Option[Chapter.Key],
  next: Option[Chapter.Key])

object ChapterProps {
  def apply(chapter: Chapter, book: Book): ChapterProps = {
    val index = book.chapters.indexOf(chapter)
    val previous = if (index > 0) Some(book.chapters(index - 1).key) else None
    val next = if (index < book.chapters.length - 1) Some(book.chapters(index + 1).key) else None
    ChapterProps(chapter.title, chapter.key, chapter.summary, chapter.entries.mapCollect(toSummary), previous, next)
  }
  private def toSummary(entry: ChapterEntry): Option[Entry] = entry match {
    case axiom: entries.Axiom =>
      import axiom._
      Some(AxiomPropsForChapter(name, key, premises, conclusion))
    case theorem: entries.Theorem =>
      import theorem._
      Some(TheoremPropsForChapter(name, key, premises, conclusion))
    case statementDefinition: entries.StatementDefinition =>
      import statementDefinition._
      Some(StatementDefinitionPropsForChapter(defaultValue, key, shorthand, definingStatement))
    case termDefinition: entries.TermDefinition =>
      import termDefinition._
      Some(TermDefinitionPropsForChapter(defaultValue, key, shorthand, definingStatement, premises))
    case comment: entries.Comment =>
      import comment._
      Some(CommentPropsForChapter(text, key))
    case _ =>
      None
  }

  sealed trait Entry {
    @JsonSerialize
    val `type`: String
  }
  case class AxiomPropsForChapter(name: String, key: ChapterEntry.Key, premises: Seq[Statement], conclusion: Statement) extends Entry {
    override val `type`: String = "axiom"
  }
  case class TheoremPropsForChapter(name: String, key: ChapterEntry.Key, premises: Seq[Statement], conclusion: Statement) extends Entry {
    override val `type`: String = "theorem"
  }
  case class StatementDefinitionPropsForChapter(defaultValue: Statement, key: ChapterEntry.Key, shorthand: Option[String], definingStatement: Option[Statement]) extends Entry {
    override val `type`: String = "statementDefinition"
  }
  case class TermDefinitionPropsForChapter(defaultValue: Term, key: ChapterEntry.Key, shorthand: Option[String], definingStatement: Statement, premises: Seq[Statement]) extends Entry {
    override val `type`: String = "termDefinition"
  }
  case class CommentPropsForChapter(text: String, key: ChapterEntry.Key) extends Entry {
    override val `type`: String = "comment"
  }
}
