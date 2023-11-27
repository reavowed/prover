import {
  DefinedExpression,
  Expression,
  ExpressionMatchResult,
  MatchResult,
  matchTemplate,
  Variable
} from "../../models/Expression";
import _ from "lodash";
import {DisplayShorthand} from "../definitions/EntryDefinitionSummaries";
import {AvailableEntries} from "../AvailableEntriesContext";
import DisplaySettings from "../DisplaySettings";
import {DisambiguatorAdder} from "../definitions/DefinitionParts";

export type DisplayShorthandMatch = {
  displayShorthand: DisplayShorthand
  matches: MatchResult[]
}
export function matchDisplayShorthand(expression: Expression, availableEntries: AvailableEntries, displaySettings: DisplaySettings): DisplayShorthandMatch | undefined {
  for (const displayShorthand of _.reverse(availableEntries.displayShorthands.slice())) {
    const matches = matchTemplate(displayShorthand.template, expression, [], []);
    if (matches) {
      const matchesConditions = _.every(displayShorthand.conditions, condition => {
        const match = _.find(matches as ExpressionMatchResult[], match => match.matchedVariable === condition[0]);
        if (match && match.expression instanceof DefinedExpression) {
          return _.includes(match.expression.definition.attributes, condition[1]);
        } else if (match && match.expression instanceof Variable) {
          return _.includes(match.expression.getDefinition(displaySettings.variableDefinitions).attributes, condition[1]);
        } else {
          return false;
        }
      });
      if (matchesConditions) return {displayShorthand, matches};
    }
  }
}

export type DisambiguatorAdderMatch = {
  disambiguatorAdder: DisambiguatorAdder
  match: MatchResult
}
export function matchDisambiguatorAdder(expression: Expression, availableEntries: AvailableEntries): DisambiguatorAdderMatch | undefined {
  for (const disambiguatorAdder of _.reverse(availableEntries.disambiguatorAdders.slice())) {
    const matches = matchTemplate(disambiguatorAdder.template, expression, [], []);
    if (matches && matches.length === 1) {
      return {disambiguatorAdder, match: matches[0]};
    }
  }
}
