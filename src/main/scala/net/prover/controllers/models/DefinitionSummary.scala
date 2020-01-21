package net.prover.controllers.models

import com.fasterxml.jackson.annotation.JsonInclude
import net.prover.controllers.models.DefinitionSummary.ComponentSummary
import net.prover.model.EntryContext
import net.prover.model.entries.ExpressionDefinition
import net.prover.model.entries.ExpressionDefinition.ComponentType

case class DefinitionSummary(symbol: String, baseFormatString: String, components: Seq[ComponentSummary], requiresBrackets: Boolean, requiresComponentBrackets: Boolean, numberOfBoundVariables: Int, attributes: Seq[String])

object DefinitionSummary {
  @JsonInclude(JsonInclude.Include.NON_ABSENT)
  case class ComponentSummary(`type`: String, arity: Option[Int])
  object ComponentSummary {
    def fromComponent(componentType: ComponentType): ComponentSummary = componentType match {
      case ComponentType.StatementComponent(_) => ComponentSummary("statement", None)
      case ComponentType.TermComponent(_) => ComponentSummary("term", None)
      case ComponentType.PredicateComponent(_, arguments) => ComponentSummary("predicate", Some(arguments.size))
      case ComponentType.FunctionComponent(_, arguments) => ComponentSummary("function", Some(arguments.size))
    }
  }

  def getAllFromContext(entryContext: EntryContext): Map[String, DefinitionSummary] = {
    entryContext.availableEntries.ofType[ExpressionDefinition]
      .map(d => d.symbol -> DefinitionSummary(
        d.symbol,
        d.format.baseFormatString,
        d.componentTypes.map(ComponentSummary.fromComponent),
        d.format.requiresBrackets,
        d.format.requiresComponentBrackets,
        d.boundVariableNames.length,
        d.attributes))
      .toMap
  }
}
