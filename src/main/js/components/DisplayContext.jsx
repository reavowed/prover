import _ from "lodash";
import * as React from "react";
import {useContext} from "react";
import {DefinedExpression, matchTemplate, TypeLikeExpression, TypeRelationExpression} from "../models/Expression";
import EntryContext from "./EntryContext";

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

function getDisambiguatorsForExpressions(expressions, entryContext) {
  return mergeDisambiguators(_.map(expressions, e => getDisambiguatorsForExpression(e, entryContext)));
}

function getBaseDisambiguator(expression, entryContext) {
  if (expression.symbol && expression.disambiguator && _.isEqual(expression.components, [])) {
    return { symbol: expression.symbol, disambiguator: expression.disambiguator };
  } else {
    for (const disambiguatorAdder of _.reverse(entryContext.disambiguatorAdders.slice())) {
      const matches = matchTemplate(disambiguatorAdder.template, expression, [], []);
      if (matches && matches.length === 1) {
        const inner = getBaseDisambiguator(matches[0].expression, entryContext);
        if (inner) {
          return { symbol: inner.symbol, disambiguator: disambiguatorAdder.disambiguator };
        }
      }
    }
  }
}

function getDisambiguatorsForExpression(expression, entryContext) {
  const base = getBaseDisambiguator(expression, entryContext);
  if (base) {
    return {[base.symbol]: [base.disambiguator]}
  } else if (expression instanceof DefinedExpression) {
    const componentDisambiguators = getDisambiguatorsForExpressions(expression.components, entryContext);
    if (expression.symbol && expression.disambiguator) {
      return mergeDisambiguators([{[expression.symbol]: [expression.disambiguator]}, componentDisambiguators]);
    } else {
      return componentDisambiguators;
    }
  } else if (expression instanceof TypeLikeExpression) {
    return getDisambiguatorsForExpressions([expression.term, ...expression.qualifierComponents], entryContext);
  } else if (expression instanceof TypeRelationExpression) {
    return getDisambiguatorsForExpressions([expression.firstTerm, expression.secondTerm], entryContext);
  }
  return {};
}

DisplayContext.forExpressionDefinition = function(definition, entryContext) {
  definition = entryContext.definitions[definition.symbol];

  const variableDefinitions = {
    statements: _.filter(definition.components, c => c.type === "statement"),
    terms: _.filter(definition.components, c => c.type === "term")
  };

  const relevantStatements = _.filter([definition.definingStatement, ...(definition.premises || [])]);
  const statementDisambiguators = getDisambiguatorsForExpressions(relevantStatements, entryContext);
  const disambiguators = definition.disambiguator ?
    mergeDisambiguators([{[definition.symbol]: [definition.disambiguator]}, statementDisambiguators]):
    statementDisambiguators;

  return DisplayContext.construct(variableDefinitions, disambiguators);
};

DisplayContext.forDefinitionWithDefiningStatement = function(definition, entryContext) {
  const disambiguators = getDisambiguatorsForExpression(definition.definingStatement, entryContext);
  return DisplayContext.construct(null, disambiguators);
};

DisplayContext.forTypeLikeDefinition = function(definingStatement, termVariableDefinitions, entryContext) {
  const variableDefinitions = {
    statements: [],
    terms: termVariableDefinitions.map(d => { return {...d, arity: 0}})
  };
  const disambiguators = getDisambiguatorsForExpression(definingStatement, entryContext);
  return DisplayContext.construct(variableDefinitions, disambiguators);
};

DisplayContext.disambiguatorsForInferenceSummary = function(inference, entryContext) {
  const expressions = [...inference.premises, inference.conclusion];
  return getDisambiguatorsForExpressions(expressions, entryContext);
};

DisplayContext.forInferenceSummary = function(inference, entryContext) {
  const disambiguators = DisplayContext.disambiguatorsForInferenceSummary(inference, entryContext);
  return DisplayContext.construct(inference.variableDefinitions, disambiguators);
};

DisplayContext.AddSteps = function({steps, children}) {
  const entryContext = useContext(EntryContext);
  const existingDisplayContext = useContext(DisplayContext);
  const statements = _.chain(steps).map(s => s.provenStatement).filter().value();
  const newDisambiguators = getDisambiguatorsForExpressions(statements, entryContext);
  return <DisplayContext.Provider value={{...existingDisplayContext, disambiguators: mergeDisambiguators([existingDisplayContext.disambiguators, newDisambiguators])}}>
    {children}
  </DisplayContext.Provider>;
};

export default DisplayContext;
