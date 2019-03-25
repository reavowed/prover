import _ from "lodash";
import React from "react";
import styled from "styled-components";
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

export function formatHtml(text, replacementFunction) {
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
  matchShorthand(template, expression, pathWithinMatch, boundVariablesWithinMatch) {
    if (_.isString(template)) {
      return {type: "expression", expression, pathWithinMatch, boundVariablesWithinMatch};
    } else if (_.isArray(template) && _.isString(template[0])) {
      if ((expression instanceof DefinedExpression) && (expression.definition.symbol === template[0])) {
        const innerBoundVariables = expression.definition.numberOfBoundVariables ? [expression.boundVariableNames, ...boundVariablesWithinMatch] : boundVariablesWithinMatch;
        const componentMatches = _.chain(template.slice(1))
          .zip(expression.components)
          .map(([t, c], i) => this.matchShorthand(t, c, [...pathWithinMatch, i], innerBoundVariables))
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

  renderMatch(match, path, pathsToHighlight, boundVariableLists, wrapBoundVariable) {
    if (match.type === "boundVariable") {
      return wrapBoundVariable(formatHtml(match.name), match.name, match.index, path.concat(match.pathWithinMatch));
    } else {
      const lengthOfPath = match.pathWithinMatch.length;
      const innerPathsToHighlight = _.chain(pathsToHighlight)
        .filter(p => _.isEqual(p.slice(0, lengthOfPath), match.pathWithinMatch))
        .map(p => p.slice(lengthOfPath))
        .value();
      return <Expression expression={match.expression}
                         path={path.concat(match.pathWithinMatch)}
                         pathsToHighlight={innerPathsToHighlight}
                         boundVariableLists={[...match.boundVariablesWithinMatch, ...boundVariableLists]}
                         wrapBoundVariable={wrapBoundVariable}
                         safe={true}/>
    }
  }

  renderInner(expression, path, pathsToHighlight, boundVariableLists, wrapBoundVariable, safe) {
    for (const shorthand of window.shorthands) {
      const matches = this.matchShorthand(shorthand.template, expression, [], []);
      if (matches) {
        let renderedMatches = matches.map(m => this.renderMatch(m, path, pathsToHighlight, boundVariableLists, wrapBoundVariable));
        let formatString = (safe && shorthand.requiresBrackets) ?
          "(" + shorthand.baseFormatString + ")" :
          shorthand.baseFormatString;
        return formatHtml(formatString, s => replacePlaceholders(s, renderedMatches));
      }
    }
    if (expression.formatForHtml) {
      const format = expression.formatForHtml(safe);
      const boundVariables = expression.boundVariableNames || [];
      const innerBoundVariables = boundVariables.length ? [boundVariables, ...boundVariableLists] : boundVariableLists;
      const renderedBoundVariables = boundVariables.map((name, index) => wrapBoundVariable(formatHtml(name), name, index, path));
      const renderedComponents = expression.components.map((c, i) => {
        const innerPaths = _.chain(pathsToHighlight).filter(p => p.length > 0 && p[0] === i).map(p => p.slice(1)).value();
        return <Expression expression={c}
                           path={[...path, i]}
                           pathsToHighlight={innerPaths}
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
    const {expression, pathsToHighlight, boundVariableLists, safe} = this.props;
    let {wrapBoundVariable, path} = this.props;
    wrapBoundVariable = wrapBoundVariable || _.identity;
    path = path || [];
    const shouldHighlightThis = _.some(pathsToHighlight, p => p.length === 0);
    const tag = shouldHighlightThis ? HighlightedSpan : React.Fragment;
    return React.createElement(tag, {}, this.renderInner(expression, path, pathsToHighlight, boundVariableLists, wrapBoundVariable, safe).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>));
  }
}

export class HighlightableStatement extends React.Component {
  render() {
    const {statement, reference, boundVariableLists, wrapBoundVariable, highlightedPremises, className} = this.props;
    let {references} = this.props;
    references = references || [reference];
    const matchingPremises = _.filter(highlightedPremises, p => references.includes(p.lineReference));
    const pathsToHighlight = _.map(matchingPremises, p => p.internalPath);
    const expression = <Expression expression={statement} pathsToHighlight={pathsToHighlight} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapBoundVariable} safe={false}/>
    return className ? <span className={className}>{expression}</span> : expression;
  }
}