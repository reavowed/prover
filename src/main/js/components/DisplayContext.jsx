import _ from "lodash";
import {useContext} from "react";
import * as React from "react";
import {matchTemplate} from "../models/Expression";
import EntryContext from "./EntryContext";

const DisplayContext = React.createContext();

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
  } else if (!_.isUndefined(expression.components)) {
    const componentDisambiguators = getDisambiguatorsForExpressions(expression.components, entryContext);
    if (expression.symbol && expression.disambiguator) {
      return mergeDisambiguators([{[expression.symbol]: [expression.disambiguator]}, componentDisambiguators]);
    } else {
      return componentDisambiguators;
    }
  } else if (expression.term && expression.qualifierComponents) {
    return getDisambiguatorsForExpressions([expression.term, ...expression.qualifierComponents], entryContext);
  }
  return {};
}

DisplayContext.fromDisambiguators = function(disambiguators) {
  return {
    disableChaining: false,
    disableShorthands: false,
    disableAssumptionCollapse: false,
    disambiguators
  };
};

DisplayContext.forExpressionDefinition = function(definition, entryContext) {
  const relevantStatements = _.filter([definition.definingStatement, ...(definition.premises || [])]);

  const statementDisambiguators = getDisambiguatorsForExpressions(relevantStatements, entryContext);
  const disambiguators = definition.disambiguator ?
    mergeDisambiguators([{[definition.symbol]: [definition.disambiguator]}, statementDisambiguators]):
    statementDisambiguators;
  return DisplayContext.fromDisambiguators(disambiguators);
};

DisplayContext.forDefinitionWithDefiningStatement = function(definition, entryContext) {
  const disambiguators = getDisambiguatorsForExpression(definition.definingStatement, entryContext);
  return DisplayContext.fromDisambiguators(disambiguators);
};

DisplayContext.disambiguatorsForInferenceSummary = function(inference, entryContext) {
  const expressions = [...inference.premises, inference.conclusion];
  return getDisambiguatorsForExpressions(expressions, entryContext);
};

DisplayContext.forInferenceSummary = function(inference, entryContext) {
  const disambiguators = DisplayContext.disambiguatorsForInferenceSummary(inference, entryContext);
  return DisplayContext.fromDisambiguators(disambiguators);
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
