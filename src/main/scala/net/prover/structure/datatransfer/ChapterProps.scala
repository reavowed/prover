package net.prover.structure.datatransfer

import net.prover.controllers.models.{StandalonePropertyDefinitionSummary, TypeDefinitionSummary}
import net.prover.model.definitions.Definitions
import net.prover.model.expressions.Statement
import net.prover.model.{DisambiguatedSymbol, Inference, VariableDefinitions}
import net.prover.shorthands.model.entries.DisplayShorthand

case class ChapterProps(
  title: String,
  url: String,
  bookLink: LinkSummary,
  summary: String,
  entries: Seq[ChapterProps.EntryProps[_]],
  previous: Option[LinkSummary],
  next: Option[LinkSummary],
  definitions: Map[String, DefinitionSummary],
  typeDefinitions: Map[String, TypeDefinitionSummary],
  standalonePropertyDefinitions: Map[String, StandalonePropertyDefinitionSummary],
  displayShorthands: Seq[DisplayShorthand],
  definitionShorthands: Map[String, DisambiguatedSymbol])

object ChapterProps {
  case class EntryProps[T](`type`: String, url: String, title: Option[String], entry: T)
  object EntryProps {
    def apply[T](`type`: String, url: String, title: String, entry: T): EntryProps[T] = EntryProps(`type`, url, Some(title), entry)
  }

  case class InferenceSummaryForChapter(name: String, variableDefinitions: VariableDefinitions, premises: Seq[Statement], conclusion: Statement, isComplete: Boolean)
  object InferenceSummaryForChapter {
    def apply(inference: Inference, definitions: Definitions): InferenceSummaryForChapter = InferenceSummaryForChapter(inference.name, inference.variableDefinitions, inference.premises, inference.conclusion, definitions.isInferenceComplete(inference))
  }
}
