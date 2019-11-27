import _ from "lodash";
import React from "react";
import {connect} from "react-redux";
import styled from "styled-components";
import {matchTemplate, PropertyExpression, TypeExpression} from "../models/Expression";
import {formatHtml, formatHtmlWithoutWrapping, replacePlaceholders} from "./helpers/Formatter";

const HighlightedPremise = styled.span`
  color: red;
`;
const HighlightedConclusion = styled.span`
  color: blue;
`;
const ClickablePremise = styled(HighlightedPremise)`
  cursor: pointer;
`;

function filterPaths(paths, initialPath) {
  return _.chain(paths)
    .filter(p => _.isEqual(p.slice(0, initialPath.length), initialPath))
    .map(p => p.slice(initialPath.length))
    .value();
}

export class ExpressionComponent extends React.Component {
  matchDisplayShorthand(displayShorthand, expression, pathWithinMatch, boundVariablesWithinMatch) {
    const matches = matchTemplate(displayShorthand.template, expression, pathWithinMatch, boundVariablesWithinMatch);
    if (matches) {
      const matchesConditions = _.every(displayShorthand.conditions, condition => {
        const match = _.find(matches, match => match.matchedVariable === condition[0]);
        return match && match.expression.definition && _.includes(match.expression.definition.attributes, condition[1]);
      });
      if (matchesConditions) return matches;
    }
  }

  renderMatch(match, path, pathsToHighlightAsPremise, highlightingAction, boundVariableLists, wrapBoundVariable) {
    if (match.type === "boundVariable") {
      return wrapBoundVariable(match.name, match.index, path.concat(match.pathWithinMatch));
    } else {
      const innerPaths = filterPaths(pathsToHighlightAsPremise, match.pathWithinMatch);
      return <ExpressionComponent expression={match.expression}
                                  path={path.concat(match.pathWithinMatch)}
                                  pathsToHighlightAsPremise={innerPaths}
                                  highlightingAction={highlightingAction}
                                  boundVariableLists={[...match.boundVariablesWithinMatch, ...boundVariableLists]}
                                  wrapBoundVariable={wrapBoundVariable}
                                  parentRequiresBrackets={true}/> // Display shorthands currently default to requiring brackets
    }
  }

  renderInner(expression, path, pathsToHighlightAsPremise, highlightingAction, boundVariableLists, wrapBoundVariable, parentRequiresBrackets) {
    for (const displayShorthand of _.reverse(window.displayShorthands.slice())) {
      const matches = this.matchDisplayShorthand(displayShorthand, expression, [], []);
      if (matches) {
        let renderedMatches = matches.map(m => this.renderMatch(m, path, pathsToHighlightAsPremise, highlightingAction, boundVariableLists, wrapBoundVariable));
        let formatString = (parentRequiresBrackets && displayShorthand.requiresBrackets) ?
          "(" + displayShorthand.baseFormatString + ")" :
          displayShorthand.baseFormatString;
        return formatHtmlWithoutWrapping(formatString, s => replacePlaceholders(s, renderedMatches));
      }
    }
    if (expression instanceof TypeExpression) {
      const formattedTerm = <ExpressionComponent expression={expression.term} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} parentRequiresBrackets={false}/>;
      const renderedOtherComponents = expression.otherComponents.map(c => <ExpressionComponent expression={c} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} parentRequiresBrackets={false}/>);
      const formattedComponents = formatHtmlWithoutWrapping(expression.definition.componentFormatString, s => replacePlaceholders(s, renderedOtherComponents));
      const formattedProperties = expression.properties.join(", ");
      const articleWord = expression.properties.length ? expression.properties[0] : expression.definition.name;
      const article = _.includes("aeiou", articleWord[0]) ? "an" : "a";

      return [formattedTerm, <> is {article} {formattedProperties} {expression.definition.name} </>, ...formattedComponents];
    } else if (expression instanceof PropertyExpression) {
      const formattedTerm = <ExpressionComponent expression={expression.term} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} parentRequiresBrackets={false}/>;
      return [formattedTerm, <> is </>, expression.name];
    } else if (expression.formatForHtml) {
      const format = expression.formatForHtml(parentRequiresBrackets);
      const boundVariables = expression.boundVariableNames || [];
      const innerBoundVariables = boundVariables.length ? [boundVariables, ...boundVariableLists] : boundVariableLists;
      const renderedBoundVariables = boundVariables.map((name, index) => wrapBoundVariable(name, index, path));
      const renderedComponents = expression.components.map((c, i) => {
        const innerPaths = filterPaths(pathsToHighlightAsPremise, [i]);
        return <ExpressionComponent expression={c}
                                    path={[...path, i]}
                                    pathsToHighlightAsPremise={innerPaths}
                                    highlightingAction={highlightingAction}
                                    boundVariableLists={innerBoundVariables}
                                    wrapBoundVariable={wrapBoundVariable}
                                    parentRequiresBrackets={expression.definition ? expression.definition.requiresComponentBrackets : true}/>
      });
      return formatHtmlWithoutWrapping(format, s => replacePlaceholders(s, [...renderedBoundVariables, ...renderedComponents]));
    } else if (expression.textForHtml) {
      return formatHtmlWithoutWrapping(expression.textForHtml(boundVariableLists));
    }
  }

  render() {
    const {expression, pathsToHighlightAsPremise, highlightingAction, boundVariableLists, parentRequiresBrackets} = this.props;
    let {wrapBoundVariable, path} = this.props;
    wrapBoundVariable = wrapBoundVariable || ((name) => formatHtml(name));
    path = path || [];
    const shouldHighlightThis = _.some(pathsToHighlightAsPremise, p => p.length === 0);
    const tag = shouldHighlightThis ? highlightingAction ? ClickablePremise : HighlightedPremise : React.Fragment;
    const props = {};
    if (shouldHighlightThis && highlightingAction) {
      props.onClick = () => highlightingAction(expression.serialize())
    }
    return React.createElement(tag, props, this.renderInner(expression, path, pathsToHighlightAsPremise, highlightingAction, boundVariableLists, wrapBoundVariable, parentRequiresBrackets).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>));
  }
}

function referencesMatch(r1, r2) {
  if (_.isNumber(r1.premiseIndex)) {
    return r1.premiseIndex === r2.premiseIndex;
  } else {
    return r2.stepPath &&
      r1.stepPath.join(".") === r2.stepPath.join(".") &&
      r1.suffix === r2.suffix;
  }
}

export const HighlightableExpression = connect(
  (state, {expression, references, additionalReferences, additionalPremiseReferences, additionalConclusionReferences, boundVariableLists, wrapBoundVariable, className}) => {

    additionalReferences = additionalReferences || [];
    additionalPremiseReferences = additionalPremiseReferences || [];
    additionalConclusionReferences = additionalConclusionReferences || [];
    let referencesAsPremise = state.highlighting.action ? [...references, ...additionalPremiseReferences] : [...references, ...additionalReferences, ...additionalPremiseReferences];
    let referencesAsConclusion = [...references, ...additionalReferences, ...additionalConclusionReferences];

    const matchingPremises = _.filter(state.highlighting.premises, highlightedPremise =>
      _.some(referencesAsPremise, reference => referencesMatch(reference, highlightedPremise))
    );
    const pathsToHighlightAsPremise = _.map(matchingPremises, p => p.internalPath || []);
    const shouldHighlightAsConclusion = !state.highlighting.action && state.highlighting.conclusion && _.some(referencesAsConclusion, r => referencesMatch(r, state.highlighting.conclusion));

    return {
      expression,
      pathsToHighlightAsPremise,
      highlightingAction: state.highlighting.action,
      shouldHighlightAsConclusion,
      boundVariableLists,
      wrapBoundVariable,
      className
    }
  }
)(({expression, pathsToHighlightAsPremise, highlightingAction, shouldHighlightAsConclusion, boundVariableLists, wrapBoundVariable, className}) => {
  const expressionElement = <ExpressionComponent expression={expression} pathsToHighlightAsPremise={pathsToHighlightAsPremise} highlightingAction={highlightingAction} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} parentRequiresBrackets={false}/>;
  return shouldHighlightAsConclusion ? <HighlightedConclusion className={className}>{expressionElement}</HighlightedConclusion> :
    className ? <span className={className}>{expressionElement}</span> : expressionElement;
});
