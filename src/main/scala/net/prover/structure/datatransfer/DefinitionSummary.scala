package net.prover.structure.datatransfer

import net.prover._
import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType
import net.prover.model.expressions.{DefinedTermTemplate, Template}
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.{CompoundExpressionDefinitionEntry, CompoundTermDefinitionEntry}

case class DefinitionSummary(
  symbol: DisambiguatedSymbol,
  baseFormatString: String,
  requiresBrackets: Boolean,
  requiresComponentBrackets: Boolean,
  numberOfBoundVariables: Int,
  components: Seq[ComponentSummary],
  attributes: Seq[String],
  disambiguatorAdders: Seq[DisambiguatorAdderSummary])

object DefinitionSummary {
  def getAllFromContext(entryContext: EntryContext): Map[String, DefinitionSummary] = {
    entryContext.availableEntries.ofType[CompoundExpressionDefinitionEntry]
      .map(d =>
        d.disambiguatedSymbol.serialized -> DefinitionSummary(
          d.disambiguatedSymbol,
          d.format.baseFormatString,
          d.format.requiresBrackets,
          d.format.requiresComponentBrackets,
          d.boundVariableNames.length,
          getComponentSummaries(d.componentTypes),
          d.attributes,
          getDisambiguatorAdderSummaries(d)))
      .toMap
  }
  def getDisambiguatorAdderSummaries(expressionDefinition: CompoundExpressionDefinitionEntry): Seq[DisambiguatorAdderSummary] = expressionDefinition match {
    case termDefinition: CompoundTermDefinitionEntry => {
      termDefinition.disambiguatorAdders.map { da =>
        DisambiguatorAdderSummary(da.template.specify(Seq(DefinedTermTemplate(termDefinition, Nil, Nil))), da.disambiguator)
      }
    }
    case _ => Nil
  }
  def getComponentSummaries(componentTypes: Seq[ComponentType]): Seq[ComponentSummary] = {
    componentTypes map {
      case ComponentType.StatementComponent(name, arguments) =>
        ComponentSummary("statement", name, arguments.length)
      case ComponentType.TermComponent(name, arguments) =>
        ComponentSummary("term", name, arguments.length)
    }
  }
  def getDefinitionShorthandsFromContext(entryContext: EntryContext): Map[String, DisambiguatedSymbol] = {
    val shorthandsFromDefinitions = entryContext.availableEntries.ofType[CompoundExpressionDefinitionEntry].mapCollect(d => d.shorthand.map(_ -> d.disambiguatedSymbol)).toMap
    val greekLetterShorthands = 'α'.to('ω')
      .map(c => Character.getName(c).splitByWhitespace().last.toLowerCase -> DisambiguatedSymbol(c.toString, None))
      .toMap
    shorthandsFromDefinitions ++ greekLetterShorthands
  }
}
