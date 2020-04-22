package net.prover.controllers.models

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.DisambiguatedSymbol
import net.prover.model.definitions.Qualifier
import net.prover.model.entries.DisplayShorthand
import net.prover.model.expressions.{Statement, Term}

case class ChapterProps(
  title: String,
  url: String,
  bookLink: LinkSummary,
  summary: String,
  entries: Seq[ChapterProps.EntryProps],
  previous: Option[LinkSummary],
  next: Option[LinkSummary],
  definitions: Map[String, DefinitionSummary],
  typeDefinitions: Map[String, TypeDefinitionSummary],
  standalonePropertyDefinitions: Map[String, StandalonePropertyDefinitionSummary],
  displayShorthands: Seq[DisplayShorthand],
  definitionShorthands: Map[String, DisambiguatedSymbol])

object ChapterProps {
  sealed trait EntryProps {
    @JsonSerialize
    val `type`: String
    @JsonSerialize
    val url: String
  }
  case class AxiomPropsForChapter(name: String, url: String, premises: Seq[Statement], conclusion: Statement) extends EntryProps {
    override val `type`: String = "axiom"
  }
  case class TheoremPropsForChapter(name: String, url: String, premises: Seq[Statement], conclusion: Statement, isComplete: Boolean) extends EntryProps {
    override val `type`: String = "theorem"
  }
  case class StatementDefinitionPropsForChapter(defaultValue: Statement, url: String, shorthand: Option[String], definingStatement: Option[Statement]) extends EntryProps {
    override val `type`: String = "statementDefinition"
  }
  case class TermDefinitionPropsForChapter(defaultValue: Term, url: String, shorthand: Option[String], definingStatement: Statement, premises: Seq[Statement]) extends EntryProps {
    override val `type`: String = "termDefinition"
  }
  case class TypeDefinitionPropsForChapter(symbol: String, url: String, defaultTermName: String, defaultQualifierTermNames: Seq[String], definingStatement: Statement) extends EntryProps {
    override val `type`: String = "typeDefinition"
  }
  case class TypeQualifierDefinitionPropsForChapter(symbol: String, url: String, parentTypeSymbol: String, defaultTermName: String, qualifierTermNames: Seq[String], definingStatement: Statement) extends EntryProps {
    override val `type`: String = "typeQualifierDefinition"
  }
  case class PropertyDefinitionPropsForChapter(symbol: String, name: String, url: String, parentTypeSymbol: String, defaultTermName: String, defaultQualifierTermNames: Option[Seq[String]], definingStatement: Statement) extends EntryProps {
    override val `type`: String = "propertyDefinition"
  }
  case class StandalonePropertyDefinitionPropsForChapter(name: String, url: String, defaultTermName: String, definingStatement: Statement) extends EntryProps {
    override val `type`: String = "standalonePropertyDefinition"
  }
  case class CommentPropsForChapter(text: String, url: String) extends EntryProps {
    override val `type`: String = "comment"
  }
  case class PlaceholderPropsForChapter(url: String) extends EntryProps {
    override val `type`: String = "placeholder"
  }
}
