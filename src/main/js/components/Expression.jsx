import React from "react";
import styled from "styled-components";
import _ from "lodash";
import {DefinedExpression, FunctionParameter} from "../Parser";

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

function formatHtml(text, replacementFunction) {
  if (!replacementFunction) {
    replacementFunction = x => <React.Fragment>{x}</React.Fragment>;
  }
  return formatWithReplacement(text, /([_^])([^\s)}]+)/g, replacementFunction, match => {
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

const HighlightedSpan = styled.span`
  color: red;
`;

export class Expression extends React.Component {
  matchShorthand(template, expression, boundVariableLists) {
    if (_.isString(template)) {
      return <Expression expression={expression} boundVariableLists={boundVariableLists}/>;
    } else if (_.isArray(template) && _.isString(template[0])) {
      if ((expression instanceof DefinedExpression) && (expression.definition.symbol === template[0])) {
        let innerBoundVariableLists = expression.boundVariableNames.length > 0 ? [expression.boundVariableNames, ...boundVariableLists] : boundVariableLists;
        const componentMatches = _.zipWith(
          template.slice(1 + expression.definition.numberOfBoundVariables),
          expression.components,
          (t, c) => this.matchShorthand(t, c, innerBoundVariableLists));
        if (_.every(componentMatches)) {
          return [...expression.boundVariableNames.map(n => formatHtml(n)), ..._.flatten(componentMatches)];
        }
      }
    } else if (_.isArray(template) && _.isNumber(template[0])) {
      if ((expression instanceof FunctionParameter) && _.isEqual(template, [expression.level, expression.index])) {
        return [];
      }
    }
  }

  renderInner(expression, pathsToHighlight, boundVariableLists, safe) {
    for (const shorthand of window.shorthands) {
      const matches = this.matchShorthand(shorthand.template, expression, boundVariableLists);
      if (matches) {
        let formatString = (safe && shorthand.requiresBrackets) ?
          "(" + shorthand.baseFormatString + ")" :
          shorthand.baseFormatString;
        return formatHtml(formatString, s => replacePlaceholders(s, matches));
      }
    }
    if (expression.formatForHtml) {
      const format = expression.formatForHtml(safe);
      const boundVariables = expression.boundVariableNames || [];
      const innerBoundVariables = boundVariables.length ? [boundVariables, ...boundVariableLists] : boundVariableLists;
      const renderedBoundVariables = boundVariables.map(v => formatHtml(v));
      const renderedComponents = expression.components.map((c, i) => {
        const innerPaths = _.chain(pathsToHighlight).filter(p => p.length > 0 && p[0] === i).map(p => p.slice(1)).value();
        return <Expression expression={c} pathsToHighlight={innerPaths} boundVariableLists={innerBoundVariables} safe={true}/>
      });
      return formatHtml(format, s => replacePlaceholders(s, [...renderedBoundVariables, ...renderedComponents]));
    } else if (expression.textForHtml) {
      return formatHtml(expression.textForHtml(boundVariableLists));
    }
  }
  render() {
    const {expression, pathsToHighlight, boundVariableLists, safe} = this.props;
    const shouldHighlightThis = _.some(pathsToHighlight, p => p.length === 0);
    const tag = shouldHighlightThis ? HighlightedSpan : React.Fragment;
    return React.createElement(tag, {}, this.renderInner(expression, pathsToHighlight, boundVariableLists, safe).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>));
  }
}

export class HighlightableStatement extends React.Component {
  render() {
    const {statement, reference, boundVariableLists, highlightedPremises, className} = this.props;
    let {references} = this.props;
    references = references || [reference];
    const matchingPremises = _.filter(highlightedPremises, p => references.includes(p.lineReference));
    const pathsToHighlight = _.map(matchingPremises, p => p.internalPath);
    const expression = <Expression expression={statement} pathsToHighlight={pathsToHighlight} boundVariableLists={boundVariableLists} safe={false}/>
    return className ? <span className={className}>{expression}</span> : expression;
  }
}