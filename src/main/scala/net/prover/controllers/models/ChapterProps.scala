package net.prover.controllers.models

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.{Book, Chapter, entries}
import net.prover.model.entries.ChapterEntry
import net.prover.model.expressions.{Statement, Term}

case class ChapterProps(
  title: String,
  url: String,
  bookLink: LinkSummary,
  summary: String,
  entries: Seq[ChapterProps.EntryProps],
  previous: Option[LinkSummary],
  next: Option[LinkSummary])

object ChapterProps {
  sealed trait EntryProps {
    @JsonSerialize
    val `type`: String
  }
  case class AxiomPropsForChapter(name: String, url: String, premises: Seq[Statement], conclusion: Statement) extends EntryProps {
    override val `type`: String = "axiom"
  }
  case class TheoremPropsForChapter(name: String, url: String, premises: Seq[Statement], conclusion: Statement) extends EntryProps {
    override val `type`: String = "theorem"
  }
  case class StatementDefinitionPropsForChapter(defaultValue: Statement, url: String, shorthand: Option[String], definingStatement: Option[Statement]) extends EntryProps {
    override val `type`: String = "statementDefinition"
  }
  case class TermDefinitionPropsForChapter(defaultValue: Term, url: String, shorthand: Option[String], definingStatement: Statement, premises: Seq[Statement]) extends EntryProps {
    override val `type`: String = "termDefinition"
  }
  case class TypeDefinitionPropsForChapter(name: String, url: String, symbol: String, components: Seq[String], format: String, definingStatement: Statement) extends EntryProps {
    override val `type`: String = "typeDefinition"
  }
  case class PropertyDefinitionPropsForChapter(name: String, url: String, symbol: String, parentTypeName: String, parentTypeComponents: Seq[String], parentTypeFormat: String, definingStatement: Statement) extends EntryProps {
    override val `type`: String = "propertyDefinition"
  }
  case class CommentPropsForChapter(text: String, key: String) extends EntryProps {
    override val `type`: String = "comment"
  }
}
