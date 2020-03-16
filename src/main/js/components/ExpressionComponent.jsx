import _ from "lodash";
import React, {useContext} from "react";
import styled from "styled-components";
import {matchTemplate, PropertyExpression, TypeExpression} from "../models/Expression";
import EntryContext from "./EntryContext";
import {formatHtml, formatHtmlWithoutWrapping, replacePlaceholders} from "./helpers/Formatter";
import BoundVariableLists from "./pages/theorem/steps/BoundVariableLists";
import ProofContext from "./pages/theorem/ProofContext";
import TheoremContext from "./pages/theorem/TheoremContext";

const HighlightedPremise = styled.span`
  color: red;
`;
const HighlightedConclusion = styled.span`
  color: blue;
`;
const ClickablePremise = styled(HighlightedPremise)`
  cursor: pointer;
`;

function filterPaths(actions, initialPath) {
  return _.chain(actions)
    .filter(a => _.startsWith(a.path, initialPath))
    .map(a => Object.assign({}, a, {path: a.path.slice(initialPath.length)}))
    .value();
}

function filterPathsMultiple(actions, initialPaths) {
  const result = [];
  for(let i = 0; i < actions.length; ++i) {
    const action = actions[i];
    for(let j = 0; j < initialPaths.length; ++j) {
      const initialPath = initialPaths[j];
      if (_.isEqual(action.path, initialPath)) {
        result.push({...action, path: []});
        break;
      }
    }
  }
  return result;
}

export function ExpressionComponent({expression, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets, wrapBoundVariable, path, entryContext}) {
  entryContext = entryContext || useContext(EntryContext);
  wrapBoundVariable = wrapBoundVariable || ((name) => formatHtml(name));

  function matchDisplayShorthand(expression) {
    for (const displayShorthand of _.reverse(entryContext.displayShorthands.slice())) {
      const matches = matchTemplate(displayShorthand.template, expression, [], []);
      if (matches) {
        const matchesConditions = _.every(displayShorthand.conditions, condition => {
          const match = _.find(matches, match => match.matchedVariable === condition[0]);
          return match && match.expression.definition && _.includes(match.expression.definition.attributes, condition[1]);
        });
        if (matchesConditions) return {displayShorthand, matches};
      }
    }
  }
  function renderMatch(match, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable) {
    if (match.type === "boundVariable") {
      return wrapBoundVariable(match.name, match.index, path.concat(match.pathWithinMatch));
    } else {
      return renderExpression(
        match.expression,
        path.concat(match.pathWithinMatch),
        filterPaths(actionHighlights, match.pathWithinMatch),
        filterPaths(staticHighlights, match.pathWithinMatch),
        [...match.boundVariablesWithinMatch, ...boundVariableLists],
        true // Display shorthands currently default to requiring brackets
      );
    }
  }
  function renderChildrenOfTag(expression, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable, parentRequiresBrackets) {
    const {displayShorthand, matches} = matchDisplayShorthand(expression) || {};
    if (matches) {
      let renderedMatches = matches.map(m => renderMatch(m, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable));
      let formatString = (parentRequiresBrackets && displayShorthand.requiresBrackets) ?
        "(" + displayShorthand.baseFormatString + ")" :
        displayShorthand.baseFormatString;
      return formatHtmlWithoutWrapping(formatString, s => replacePlaceholders(s, renderedMatches));
    }

    if (expression instanceof TypeExpression) {
      function getPropertyPath(propertyIndex) {
        return [..._.times(expression.properties.length - 1 - propertyIndex, _.constant(0)), 1];
      }

      const typePath = _.times(expression.properties.length, _.constant(0));
      const typeActionHighlights = filterPaths(actionHighlights, typePath);
      const typeStaticHighlights = filterPaths(staticHighlights, typePath);
      const termPath = [...typePath, 0];
      const componentPaths = _.map(_.range(expression.properties.length), getPropertyPath);
      const sharedActionHighlights = filterPathsMultiple(actionHighlights, [typePath, ...componentPaths]);
      const sharedStaticHighlights = filterPathsMultiple(staticHighlights, [typePath, ...componentPaths]);

      const formattedTerm = renderExpression(expression.term, termPath, [...sharedActionHighlights, ...filterPaths(actionHighlights, termPath)], [...sharedStaticHighlights, ...filterPaths(staticHighlights, termPath)], boundVariableLists, false);
      const formattedIs = renderExpression({textForHtml: () => "is"}, [], sharedActionHighlights, sharedStaticHighlights, boundVariableLists, false);
      const articleWord = expression.properties.length ? expression.properties[0] : expression.definition.name;
      const article = _.includes("aeiou", articleWord[0]) ? "an" : "a";
      const formattedArticle = renderExpression({textForHtml: () => article}, [], typeActionHighlights, typeStaticHighlights, boundVariableLists, false);
      const formattedName = renderExpression({textForHtml: () => expression.definition.name}, [], typeActionHighlights, typeStaticHighlights, boundVariableLists, false);

      const formattedComponents = _.map(expression.otherComponents, (component, i) => {
        const componentPath = [...typePath, i + 1];
        return renderExpression(component, componentPath, [...sharedActionHighlights, ...filterPaths(actionHighlights, componentPath)], [...sharedStaticHighlights, ...filterPaths(staticHighlights, componentPath)], boundVariableLists, false);
      });
      const formattedComponentText = formatHtmlWithoutWrapping(expression.definition.componentFormatString, s => replacePlaceholders(s, formattedComponents));

      const formattedProperties = _.flatMap(expression.properties, (p, i) => {
        const formattedProperty = renderExpression({textForHtml: () => p}, [], filterPaths(actionHighlights, getPropertyPath(i)), filterPaths(staticHighlights, getPropertyPath(i)), boundVariableLists, false);
        if (i === 0)
          return [formattedProperty];
        else
          return [<>, </>, formattedProperty];
      });
      return [formattedTerm, <> </>, formattedIs, <> </>, formattedArticle, <> </>, ...formattedProperties, <> </>, formattedName, <> </>, formattedComponentText];
    } else if (expression instanceof PropertyExpression) {
      const formattedTerm = <ExpressionComponent expression={expression.term} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} parentRequiresBrackets={false} entryContext={entryContext}/>;
      return [formattedTerm, <> is </>, expression.name];
    } else if (expression.formatForHtml) {
      const format = expression.formatForHtml(parentRequiresBrackets);
      const boundVariables = expression.boundVariableNames || [];
      const innerBoundVariables = boundVariables.length ? [boundVariables, ...boundVariableLists] : boundVariableLists;
      const renderedBoundVariables = boundVariables.map((name, index) => wrapBoundVariable(name, index, path));
      const renderedComponents = expression.components.map((c, i) =>
        renderExpression(c, [...path, i], filterPaths(actionHighlights, [i]), filterPaths(staticHighlights, [i]), innerBoundVariables, expression.definition ? expression.definition.requiresComponentBrackets : true)
      );
      return formatHtmlWithoutWrapping(format, s => replacePlaceholders(s, [...renderedBoundVariables, ...renderedComponents]));
    } else if (expression.textForHtml) {
      return formatHtmlWithoutWrapping(expression.textForHtml(boundVariableLists));
    } else {
      throw "Could not render expression " + expression;
    }
  }

  function renderExpression(expression, path, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets) {
    const matchingActionHighlight = _.find(actionHighlights, p => p.path.length === 0);
    const shouldStaticHighlight = _.some(staticHighlights, p => p.path.length === 0);

    const tag =
      shouldStaticHighlight ? HighlightedConclusion :
        matchingActionHighlight ? (matchingActionHighlight.action ? ClickablePremise : HighlightedPremise) :
          React.Fragment;
    const props = {};
    if (!shouldStaticHighlight && matchingActionHighlight && matchingActionHighlight.action) {
      props.onClick = (e) => {
        matchingActionHighlight.action(expression.serialize());
        e.stopPropagation();
      }
    }
    return React.createElement(tag, props, renderChildrenOfTag(expression, path, actionHighlights || [], staticHighlights || [], boundVariableLists || [], wrapBoundVariable, parentRequiresBrackets).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>));
  }
  path = path || [];

  return renderExpression(expression, path, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets);
}

export const CopiableExpression = (props) => {
  const expressionToCopy = props.expressionToCopy || props.expression;
  const boundVariableLists = props.boundVariableLists || useContext(BoundVariableLists) || [];
  return <span onContextMenu={() => navigator.clipboard.writeText(expressionToCopy.serializeNicely(boundVariableLists))}>
      <ExpressionComponent {...props} boundVariableLists={boundVariableLists}/>
    </span>
};

export class HighlightableExpression extends React.Component {
  shouldComponentUpdate(nextProps, nextState, nextContext) {
    return !_.isEqual(this.props, nextProps);
  }
  render() {
    let {expression, references, additionalReferences, additionalPremiseReferences, additionalConclusionReferences, wrapBoundVariable, className, expressionToCopy} = this.props;
    additionalReferences = additionalReferences || [];
    additionalPremiseReferences = additionalPremiseReferences || [];
    additionalConclusionReferences = additionalConclusionReferences || [];
    let referencesForAction = [...references, ...additionalPremiseReferences];
    let referencesForPremise = [...references, ...additionalPremiseReferences, ...additionalReferences];
    let referencesForConclusion = [...references, ...additionalReferences, ...additionalConclusionReferences];

    function renderFromContext(context) {
      const [allActionHighlights, allStaticHighlights] = context.getHighlighting();
      const actionHighlights = _.chain(allActionHighlights)
        .filter(actionHighlight => {
          const references = actionHighlight.action ? referencesForAction : referencesForPremise;
          return _.some(references, reference => reference.matches(actionHighlight.reference))
        })
        .map(actionHighlight => {
          return {path: actionHighlight.reference.innerPath || [], action: actionHighlight.action}
        })
        .value();
      const staticHighlights = _.chain(allStaticHighlights)
        .filter(staticHighlight => _.some(referencesForConclusion, reference => reference.matches(staticHighlight)))
        .map(staticHighlight => {
          return {path: staticHighlight.innerPath || []}
        })
        .value();
      const expressionElement = <CopiableExpression expression={expression}
                                                    actionHighlights={actionHighlights}
                                                    staticHighlights={staticHighlights}
                                                    wrapBoundVariable={wrapBoundVariable}
                                                    expressionToCopy={expressionToCopy}
                                                    parentRequiresBrackets={false}/>;
      return className ? <span className={className}>{expressionElement}</span> : expressionElement;
    }

    return <ProofContext.Consumer>{ proofContext =>
      proofContext ? renderFromContext(proofContext) :
        <TheoremContext.Consumer>{theoremContext =>
          renderFromContext(theoremContext)
        }</TheoremContext.Consumer>
    }</ProofContext.Consumer>


  }
}
