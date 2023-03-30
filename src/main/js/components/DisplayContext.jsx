import _ from "lodash";
import * as React from "react";
import {useContext} from "react";
import {DefinedExpression, matchTemplate, TypeLikeExpression, TypeRelationExpression} from "../models/Expression";
import AvailableEntries from "./AvailableEntries";

const DisplayContext = React.createContext();

const DisplayContextClass = class DisplayContext {
  constructor(variableDefinitions, disambiguators, disableChaining, disableShorthands, disableAssumptionCollapse) {
    this.variableDefinitions = variableDefinitions;
    this.disambiguators = disambiguators;
    this.disableChaining = disableChaining;
    this.disableShorthands = disableShorthands;
    this.disableAssumptionCollapse = disableAssumptionCollapse;
  }

  withVariableDefinitions = (newVariableDefinitions) => {
    return new DisplayContextClass(
      newVariableDefinitions,
      this.disambiguators,
      this.disableChaining,
      this.disableShorthands,
      this.disableAssumptionCollapse);
  };

  addTermVariables = (termVariableNames) => {
    return this.withVariableDefinitions(
      {
        statements: this.variableDefinitions.statements,
        terms: [...this.variableDefinitions.terms, ...termVariableNames.map(name => {return {name, arity: 0}})]
      });
  };
};

DisplayContext.construct = function(variableDefinitions, disambiguators, disableChaining = false, disableShorthands = false, disableAssumptionCollapse = false) {
  return new DisplayContextClass(variableDefinitions, disambiguators, disableChaining, disableShorthands, disableAssumptionCollapse)
};

function mergeDisambiguators(results) {
  const keys = _.uniq(_.flatMap(results, r => _.keys(r)));
  return _.fromPairs(_.map(keys, k => [k, _.uniq(_.flatten(_.filter(_.map(results, k))))]));
}

function getDisambiguatorsForExpressions(expressions, availableEntries) {
  return mergeDisambiguators(_.map(expressions, e => getDisambiguatorsForExpression(e, availableEntries)));
}

function getBaseDisambiguator(expression, availableEntries) {
  if (expression.symbol && expression.disambiguator && _.isEqual(expression.components, [])) {
    return { symbol: expression.symbol, disambiguator: expression.disambiguator };
  } else {
    for (const disambiguatorAdder of _.reverse(availableEntries.disambiguatorAdders.slice())) {
      const matches = matchTemplate(disambiguatorAdder.template, expression, [], []);
      if (matches && matches.length === 1) {
        const inner = getBaseDisambiguator(matches[0].expression, availableEntries);
        if (inner) {
          return { symbol: inner.symbol, disambiguator: disambiguatorAdder.disambiguator };
        }
      }
    }
  }
}

function getDisambiguatorsForExpression(expression, availableEntries) {
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

DisplayContext.forExpressionDefinition = function(definition, availableEntries) {
  definition = availableEntries.definitions[definition.symbol];

  const variableDefinitions = {
    statements: _.filter(definition.components, c => c.type === "statement"),
    terms: _.filter(definition.components, c => c.type === "term")
  };

  const relevantStatements = _.filter([definition.definingStatement, ...(definition.premises || [])]);
  const statementDisambiguators = getDisambiguatorsForExpressions(relevantStatements, availableEntries);
  const disambiguators = definition.disambiguator ?
    mergeDisambiguators([{[definition.symbol]: [definition.disambiguator]}, statementDisambiguators]):
    statementDisambiguators;

  return DisplayContext.construct(variableDefinitions, disambiguators);
};

DisplayContext.forDefinitionWithDefiningStatement = function(definition, availableEntries) {
  const disambiguators = getDisambiguatorsForExpression(definition.definingStatement, availableEntries);
  return DisplayContext.construct(null, disambiguators);
};

DisplayContext.forTypeLikeDefinition = function(definingStatement, termVariableDefinitions, availableEntries) {
  const variableDefinitions = {
    statements: [],
    terms: termVariableDefinitions.map(d => { return {...d, arity: 0}})
  };
  const disambiguators = getDisambiguatorsForExpression(definingStatement, availableEntries);
  return DisplayContext.construct(variableDefinitions, disambiguators);
};

DisplayContext.disambiguatorsForInferenceSummary = function(inference, availableEntries) {
  const expressions = [...inference.premises, inference.conclusion];
  return getDisambiguatorsForExpressions(expressions, availableEntries);
};

DisplayContext.forInferenceSummary = function(inference, availableEntries) {
  const disambiguators = DisplayContext.disambiguatorsForInferenceSummary(inference, availableEntries);
  return DisplayContext.construct(inference.variableDefinitions, disambiguators);
};

DisplayContext.AddSteps = function({steps, children}) {
  const availableEntries = useContext(AvailableEntries);
  const existingDisplayContext = useContext(DisplayContext);
  const statements = _.chain(steps).map(s => s.provenStatement).filter().value();
  const newDisambiguators = getDisambiguatorsForExpressions(statements, availableEntries);
  return <DisplayContext.Provider value={{...existingDisplayContext, disambiguators: mergeDisambiguators([existingDisplayContext.disambiguators, newDisambiguators])}}>
    {children}
  </DisplayContext.Provider>;
};

export default DisplayContext;
