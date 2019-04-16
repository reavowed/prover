import _ from "lodash";
import React from "react";
import styled from "styled-components";
import {DefinedExpression, FunctionParameter, PropertyExpression, TypeExpression} from "../models/Expression";
import {formatHtml, formatHtmlWithoutWrapping, replacePlaceholders} from "./helpers/Formatter";

const HighlightedPremise = styled.span`
  color: red;
`;
const HighlightedConclusion = styled.span`
  color: blue;
`;

export class ExpressionComponent extends React.Component {
  matchTemplate(template, expression, pathWithinMatch, boundVariablesWithinMatch) {
    if (_.isString(template)) {
      return {type: "expression", matchedVariable: template, expression, pathWithinMatch, boundVariablesWithinMatch};
    } else if (_.isArray(template) && _.isString(template[0])) {
      if ((expression instanceof DefinedExpression) && (expression.definition.symbol === template[0])) {
        const innerBoundVariables = expression.definition.numberOfBoundVariables ? [expression.boundVariableNames, ...boundVariablesWithinMatch] : boundVariablesWithinMatch;
        const componentMatches = _.chain(template.slice(1))
          .zip(expression.components)
          .map(([t, c], i) => this.matchTemplate(t, c, [...pathWithinMatch, i], innerBoundVariables))
          .value();
        if (_.every(componentMatches)) {
          return [...expression.boundVariableNames.map((name, index) => ({type: "boundVariable", name, index, pathWithinMatch})), ..._.flatten(componentMatches)];
        }
      }
    } else if (_.isArray(template) && _.isNumber(template[0])) {
      if ((expression instanceof FunctionParameter) && _.isEqual(template, [expression.level, expression.index])) {
        return [];
      }
    }
  }
  matchDisplayShorthand(displayShorthand, expression, pathWithinMatch, boundVariablesWithinMatch) {
    const matches = this.matchTemplate(displayShorthand.template, expression, pathWithinMatch, boundVariablesWithinMatch);
    if (matches) {
      const matchesConditions = _.every(displayShorthand.conditions, condition => {
        const match = _.find(matches, match => match.matchedVariable === condition[0]);
        return match && match.expression.definition && _.includes(match.expression.definition.attributes, condition[1]);
      });
      if (matchesConditions) return matches;
    }
  }

  renderMatch(match, path, pathsToHighlightAsPremise, boundVariableLists, wrapBoundVariable) {
    if (match.type === "boundVariable") {
      return wrapBoundVariable(formatHtml(match.name), match.name, match.index, path.concat(match.pathWithinMatch));
    } else {
      const lengthOfPath = match.pathWithinMatch.length;
      const innerPathsToHighlightAsPremise = _.chain(pathsToHighlightAsPremise)
        .filter(p => _.isEqual(p.slice(0, lengthOfPath), match.pathWithinMatch))
        .map(p => p.slice(lengthOfPath))
        .value();
      return <ExpressionComponent expression={match.expression}
                                  path={path.concat(match.pathWithinMatch)}
                                  pathsToHighlightAsPremise={innerPathsToHighlightAsPremise}
                                  boundVariableLists={[...match.boundVariablesWithinMatch, ...boundVariableLists]}
                                  wrapBoundVariable={wrapBoundVariable}
                                  parentRequiresBrackets={true}/> // Display shorthands currently default to requiring brackets
    }
  }

  renderInner(expression, path, pathsToHighlightAsPremise, boundVariableLists, wrapBoundVariable, parentRequiresBrackets) {
    for (const displayShorthand of window.displayShorthands) {
      const matches = this.matchDisplayShorthand(displayShorthand, expression, [], []);
      if (matches) {
        let renderedMatches = matches.map(m => this.renderMatch(m, path, pathsToHighlightAsPremise, boundVariableLists, wrapBoundVariable));
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
      return [formattedTerm, <> is a {formattedProperties} {expression.definition.name} </>, ...formattedComponents];
    } else if (expression instanceof PropertyExpression) {
      const formattedTerm = <ExpressionComponent expression={expression.term} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} parentRequiresBrackets={false}/>;
      return [formattedTerm, <> is </>, expression.name];
    } else if (expression.formatForHtml) {
      const format = expression.formatForHtml(parentRequiresBrackets);
      const boundVariables = expression.boundVariableNames || [];
      const innerBoundVariables = boundVariables.length ? [boundVariables, ...boundVariableLists] : boundVariableLists;
      const renderedBoundVariables = boundVariables.map((name, index) => wrapBoundVariable(formatHtml(name), name, index, path));
      const renderedComponents = expression.components.map((c, i) => {
        const innerPaths = _.chain(pathsToHighlightAsPremise).filter(p => p.length > 0 && p[0] === i).map(p => p.slice(1)).value();
        return <ExpressionComponent expression={c}
                                    path={[...path, i]}
                                    pathsToHighlightAsPremise={innerPaths}
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
    const {expression, pathsToHighlightAsPremise, boundVariableLists, parentRequiresBrackets} = this.props;
    let {wrapBoundVariable, path} = this.props;
    wrapBoundVariable = wrapBoundVariable || _.identity;
    path = path || [];
    const shouldHighlightThis = _.some(pathsToHighlightAsPremise, p => p.length === 0);
    const tag = shouldHighlightThis ? HighlightedPremise : React.Fragment;
    return React.createElement(tag, {}, this.renderInner(expression, path, pathsToHighlightAsPremise, boundVariableLists, wrapBoundVariable, parentRequiresBrackets).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>));
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

export class HighlightableExpression extends React.Component {
  render() {
    const {statement, expression, references, reference, boundVariableLists, wrapBoundVariable, highlighting, className} = this.props;
    let {referencesAsPremise, referencesAsConclusion} = this.props;
    let defaultReferences = references || [reference];
    referencesAsPremise = referencesAsPremise || defaultReferences;
    referencesAsConclusion = referencesAsConclusion || defaultReferences;

    const matchingPremises = highlighting ? _.filter(highlighting.highlightedPremises, highlightedPremise =>
      _.some(referencesAsPremise, reference =>
        referencesMatch(reference, highlightedPremise)
      )
    ) : [];
    const pathsToHighlightAsPremise = _.map(matchingPremises, p => p.internalPath || []);

    const shouldHighlightAsConclusion = highlighting && highlighting.highlightedConclusion &&
      _.some(referencesAsConclusion, r => referencesMatch(r, highlighting.highlightedConclusion));

    const expressionElement = <ExpressionComponent expression={statement || expression} pathsToHighlightAsPremise={pathsToHighlightAsPremise} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} parentRequiresBrackets={false}/>;
    return shouldHighlightAsConclusion ? <HighlightedConclusion className={className}>{expressionElement}</HighlightedConclusion> :
      className ? <span className={className}>{expressionElement}</span> : expressionElement;
  }
}
