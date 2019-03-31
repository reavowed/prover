import _ from "lodash";
import React from "react";
import styled from "styled-components";
import {DefinedExpression, FunctionParameter} from "../models/Expression";

function formatWithReplacement(text, regex, handlePlain, handleMatch) {
  const matches = text.matchAll(regex);
  let indexOfLastMatchEnd = 0;
  let elements = [];
  for (const match of matches) {
    elements.push(handlePlain(text.substr(indexOfLastMatchEnd, match.index - indexOfLastMatchEnd)));
    elements.push(handleMatch(match));
    indexOfLastMatchEnd = match.index + match[0].length;
  }
  elements.push(handlePlain(text.substr(indexOfLastMatchEnd)));
  return _.flatten(elements);
}

export function formatHtml(text, replacementFunction) {
  if (!replacementFunction) {
    replacementFunction = x => <React.Fragment>{x}</React.Fragment>;
  }
  return formatWithReplacement(text, /(?<!\s)([_^])([^\s)}]+)/g, replacementFunction, match => {
    if (match[1] === "_") {
      return <sub>{match[2]}</sub>
    } else if (match[1] === "^") {
      return <sup>{match[2]}</sup>
    }
  });
}

function replacePlaceholders(text, components) {
  return formatWithReplacement(text, /%(\d+)/g, x => x,  match => {
    const index = parseInt(match[1]);
    return components[index];
  });
}

const HighlightedPremise = styled.span`
  color: red;
`;
const HighlightedConclusion = styled.span`
  color: blue;
`;

export class ExpressionComponent extends React.Component {
  matchDisplayShorthand(template, expression, pathWithinMatch, boundVariablesWithinMatch) {
    if (_.isString(template)) {
      return {type: "expression", expression, pathWithinMatch, boundVariablesWithinMatch};
    } else if (_.isArray(template) && _.isString(template[0])) {
      if ((expression instanceof DefinedExpression) && (expression.definition.symbol === template[0])) {
        const innerBoundVariables = expression.definition.numberOfBoundVariables ? [expression.boundVariableNames, ...boundVariablesWithinMatch] : boundVariablesWithinMatch;
        const componentMatches = _.chain(template.slice(1))
          .zip(expression.components)
          .map(([t, c], i) => this.matchDisplayShorthand(t, c, [...pathWithinMatch, i], innerBoundVariables))
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
                                  safe={true}/>
    }
  }

  renderInner(expression, path, pathsToHighlightAsPremise, boundVariableLists, wrapBoundVariable, safe) {
    for (const displayShorthand of window.displayShorthands) {
      const matches = this.matchDisplayShorthand(displayShorthand.template, expression, [], []);
      if (matches) {
        let renderedMatches = matches.map(m => this.renderMatch(m, path, pathsToHighlightAsPremise, boundVariableLists, wrapBoundVariable));
        let formatString = (safe && displayShorthand.requiresBrackets) ?
          "(" + displayShorthand.baseFormatString + ")" :
          displayShorthand.baseFormatString;
        return formatHtml(formatString, s => replacePlaceholders(s, renderedMatches));
      }
    }
    if (expression.formatForHtml) {
      const format = expression.formatForHtml(safe);
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
                                    safe={true}/>
      });
      return formatHtml(format, s => replacePlaceholders(s, [...renderedBoundVariables, ...renderedComponents]));
    } else if (expression.textForHtml) {
      return formatHtml(expression.textForHtml(boundVariableLists));
    }
  }

  render() {
    const {expression, pathsToHighlightAsPremise, boundVariableLists, safe} = this.props;
    let {wrapBoundVariable, path} = this.props;
    wrapBoundVariable = wrapBoundVariable || _.identity;
    path = path || [];
    const shouldHighlightThis = _.some(pathsToHighlightAsPremise, p => p.length === 0);
    const tag = shouldHighlightThis ? HighlightedPremise : React.Fragment;
    return React.createElement(tag, {}, this.renderInner(expression, path, pathsToHighlightAsPremise, boundVariableLists, wrapBoundVariable, safe).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>));
  }
}

export class HighlightableExpression extends React.Component {
  render() {
    const {statement, expression, references, reference, boundVariableLists, wrapBoundVariable, highlighting, className} = this.props;
    let {referencesAsPremise, referencesAsConclusion} = this.props;
    let defaultReferences = references || [reference];
    referencesAsPremise = referencesAsPremise || defaultReferences;
    referencesAsConclusion = referencesAsConclusion || defaultReferences;
    const matchingPremises = highlighting ? _.filter(highlighting.highlightedPremises, p => referencesAsPremise.includes(p.lineReference)) : [];
    const pathsToHighlightAsPremise = _.map(matchingPremises, p => p.internalPath);
    const shouldHighlightAsConclusion = highlighting && _.some(referencesAsConclusion, r => r === highlighting.highlightedConclusion);
    const expressionElement = <ExpressionComponent expression={statement || expression} pathsToHighlightAsPremise={pathsToHighlightAsPremise} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} safe={false}/>;
    return shouldHighlightAsConclusion ? <HighlightedConclusion className={className}>{expressionElement}</HighlightedConclusion> :
      className ? <span className={className}>{expressionElement}</span> : expressionElement;
  }
}
