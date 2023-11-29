import _ from "lodash";
import * as React from "react";
import {useContext} from "react";
import {
  DefinedExpression,
  Expression,
  ExpressionMatchResult,
  matchTemplate,
  TypeLikeExpression,
  TypeRelationExpression,
  Variable
} from "../models/Expression";
import AvailableEntriesContext, {AvailableEntries} from "./AvailableEntriesContext";
import {VariableDefinition, VariableDefinitions} from "./definitions/DefinitionParts";
import {ExpressionDefinition, Inference} from "./definitions/EntryDefinitions";
import {isDefined} from "../utils";
import {Step} from "../models/Step";

export const DisplaySettingsContext = React.createContext<DisplaySettings>({} as DisplaySettings);

class DisplaySettings {
  constructor(
      public variableDefinitions: VariableDefinitions,
      public disambiguators: {[key: string]: string[]},
      public disableChaining = false,
      public disableShorthands = false,
      public disableAssumptionCollapse = false
  ) {}

  withVariableDefinitions(newVariableDefinitions: VariableDefinitions) {
    return new DisplaySettings(
      newVariableDefinitions,
      this.disambiguators,
      this.disableChaining,
      this.disableShorthands,
      this.disableAssumptionCollapse);
  }

  addTermVariables(termVariableNames: string[]) {
    return this.withVariableDefinitions(
      {
        statements: this.variableDefinitions.statements,
        terms: [...this.variableDefinitions.terms, ...termVariableNames.map(name => {return {name, arity: 0}})]
      });
  }

  addDisambiguators(newDisambiguators: {[key: string]: string[]}) {
    return new DisplaySettings(
      this.variableDefinitions,
      mergeDisambiguators([this.disambiguators, newDisambiguators]),
      this.disableChaining,
      this.disableShorthands,
      this.disableAssumptionCollapse);
  }

  static disambiguatorsForInferenceSummary(inference: Inference, availableEntries: AvailableEntries) {
    const expressions = [...inference.premises, inference.conclusion];
    return getDisambiguatorsForExpressions(expressions, availableEntries);
  }

  static forExpressionDefinition(definition: ExpressionDefinition, availableEntries: AvailableEntries) {
    const summary = availableEntries.definitions[definition.symbol];
    const variableDefinitions: VariableDefinitions = {
      statements: _.filter(summary.components, c => c.type === "statement"),
      terms: _.filter(summary.components, c => c.type === "term")
    };

    const relevantStatements = [definition.definingStatement, ...(definition.premises || [])].filter(isDefined);
    const statementDisambiguators = getDisambiguatorsForExpressions(relevantStatements, availableEntries);
    const disambiguators = definition.disambiguator ?
      mergeDisambiguators([{[definition.symbol]: [definition.disambiguator]}, statementDisambiguators]):
      statementDisambiguators;

    return new DisplaySettings(variableDefinitions, disambiguators);
  }

  static forTypeLikeDefinition(definingStatement: Expression, termVariableDefinitions: VariableDefinition[], availableEntries: AvailableEntries) {
    const variableDefinitions: VariableDefinitions = {
      statements: [],
      terms: termVariableDefinitions.map(d => { return {...d, arity: 0}})
    };
    const disambiguators = getDisambiguatorsForExpression(definingStatement, availableEntries);
    return new DisplaySettings(variableDefinitions, disambiguators);
  };

  static forInferenceSummary = function(inference: Inference, availableEntries: AvailableEntries) {
    const disambiguators = DisplaySettings.disambiguatorsForInferenceSummary(inference, availableEntries);
    return new DisplaySettings(inference.variableDefinitions, disambiguators);
  };

  static AddSteps({steps, children}: {steps: Step[], children: React.ReactNode}) {
    const availableEntries = useContext(AvailableEntriesContext);
    const existingDisplaySettings = useContext(DisplaySettingsContext);
    const statements = steps.map(s => s.provenStatement).filter(isDefined);
    const newDisambiguators = getDisambiguatorsForExpressions(statements, availableEntries);
    return <DisplaySettingsContext.Provider value={existingDisplaySettings.addDisambiguators(newDisambiguators)}>
      {children}
    </DisplaySettingsContext.Provider>;
  };
}

function mergeDisambiguators(results: {[key: string]: string[]}[]): {[key: string]: string[]} {
  const keys = _.uniq(_.flatMap(results, r => _.keys(r)));
  return _.fromPairs(_.map(keys, k => [k, _.uniq(_.flatten(_.filter(_.map(results, k))))]));
}

function getDisambiguatorsForExpressions(expressions: Expression[], availableEntries: AvailableEntries) {
  return mergeDisambiguators(_.map(expressions, e => getDisambiguatorsForExpression(e, availableEntries)));
}

type Disambiguator = {
  symbol: string
  disambiguator: string
}

function getBaseDisambiguator(expression: Expression, availableEntries: AvailableEntries): Disambiguator | null {
  if ((expression instanceof Variable || expression instanceof DefinedExpression) && expression.disambiguator && _.isEqual(expression.components, [])) {
    return { symbol: expression.symbol, disambiguator: expression.disambiguator };
  } else {
    for (const disambiguatorAdder of _.reverse(availableEntries.disambiguatorAdders.slice())) {
      const matches = matchTemplate(disambiguatorAdder.template, expression, [], []);
      if (matches && matches.length === 1) {
        const inner = getBaseDisambiguator((matches[0] as ExpressionMatchResult).expression, availableEntries);
        if (inner) {
          return { symbol: inner.symbol, disambiguator: disambiguatorAdder.disambiguator };
        }
      }
    }
    return null;
  }
}

function getDisambiguatorsForExpression(expression: Expression, availableEntries: AvailableEntries): {[key: string]: string[]}  {
  const base = getBaseDisambiguator(expression, availableEntries);
  if (base) {
    return {[base.symbol]: [base.disambiguator]}
  } else if (expression instanceof DefinedExpression) {
    const componentDisambiguators = getDisambiguatorsForExpressions(expression.components, availableEntries);
    if (expression.symbol && expression.disambiguator) {
      return mergeDisambiguators([{[expression.symbol]: [expression.disambiguator]}, componentDisambiguators]);
    } else {
      return componentDisambiguators;
    }
  } else if (expression instanceof TypeLikeExpression) {
    return getDisambiguatorsForExpressions([expression.term, ...expression.qualifierComponents], availableEntries);
  } else if (expression instanceof TypeRelationExpression) {
    return getDisambiguatorsForExpressions([expression.firstTerm, expression.secondTerm], availableEntries);
  }
  return {};
}

export default DisplaySettings;
