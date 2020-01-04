import _ from "lodash";
import React, {useContext} from "react";
import {connect} from "react-redux";
import styled from "styled-components";
import {matchTemplate, PropertyExpression, TypeExpression} from "../models/Expression";
import {formatHtml, formatHtmlWithoutWrapping, replacePlaceholders} from "./helpers/Formatter";
import BoundVariableLists from "./steps/BoundVariableLists";

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
    .filter(p => _.isEqual(p.path.slice(0, initialPath.length), initialPath))
    .map(p => Object.assign({}, p, {path: p.path.slice(initialPath.length)}))
    .value();
}

export class ExpressionComponent extends React.Component {
  static matchDisplayShorthand(expression) {
    for (const displayShorthand of _.reverse(window.displayShorthands.slice())) {
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

  renderMatch(match, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable) {
    if (match.type === "boundVariable") {
      return wrapBoundVariable(match.name, match.index, path.concat(match.pathWithinMatch));
    } else {
      return <ExpressionComponent expression={match.expression}
                                  path={path.concat(match.pathWithinMatch)}
                                  actionHighlights={filterPaths(actionHighlights, match.pathWithinMatch)}
                                  staticHighlights={filterPaths(staticHighlights, match.pathWithinMatch)}
                                  boundVariableLists={[...match.boundVariablesWithinMatch, ...boundVariableLists]}
                                  wrapBoundVariable={wrapBoundVariable}
                                  parentRequiresBrackets={true}/> // Display shorthands currently default to requiring brackets
    }
  }

  renderInner(expression, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable, parentRequiresBrackets) {
    const {displayShorthand, matches} = ExpressionComponent.matchDisplayShorthand(expression) || {};
    if (matches) {
      let renderedMatches = matches.map(m => this.renderMatch(m, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable));
      let formatString = (parentRequiresBrackets && displayShorthand.requiresBrackets) ?
        "(" + displayShorthand.baseFormatString + ")" :
        displayShorthand.baseFormatString;
      return formatHtmlWithoutWrapping(formatString, s => replacePlaceholders(s, renderedMatches));
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
        return <ExpressionComponent expression={c}
                                    path={[...path, i]}
                                    actionHighlights={filterPaths(actionHighlights, [i])}
                                    staticHighlights={filterPaths(staticHighlights, [i])}
                                    boundVariableLists={innerBoundVariables}
                                    wrapBoundVariable={wrapBoundVariable}
                                    parentRequiresBrackets={expression.definition ? expression.definition.requiresComponentBrackets : true}/>
      });
      return formatHtmlWithoutWrapping(format, s => replacePlaceholders(s, [...renderedBoundVariables, ...renderedComponents]));
    } else if (expression.textForHtml) {
      return formatHtmlWithoutWrapping(expression.textForHtml(boundVariableLists));
    } else {
      throw "Could not render expression " + expression;
    }
  }

  render() {
    const {expression, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets} = this.props;
    let {wrapBoundVariable, path} = this.props;
    wrapBoundVariable = wrapBoundVariable || ((name) => formatHtml(name));
    path = path || [];

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
    return React.createElement(tag, props, this.renderInner(expression, path, actionHighlights, staticHighlights, boundVariableLists || [], wrapBoundVariable, parentRequiresBrackets).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>));
  }
}

export const CopiableExpression = (props) => {
  const expressionToCopy = props.expressionToCopy || props.expression;
  const boundVariableLists = useContext(BoundVariableLists) || [];
  return <span onContextMenu={() => navigator.clipboard.writeText(expressionToCopy.serializeNicely(boundVariableLists))}>
      <ExpressionComponent {...props} boundVariableLists={boundVariableLists}/>
    </span>
};

export const HighlightableExpression = connect(
  (state, {expression, references, additionalReferences, additionalPremiseReferences, additionalConclusionReferences, boundVariableLists, wrapBoundVariable, className, expressionToCopy}) => {

    additionalReferences = additionalReferences || [];
    additionalPremiseReferences = additionalPremiseReferences || [];
    additionalConclusionReferences = additionalConclusionReferences || [];
    let referencesForAction = [...references, ...additionalPremiseReferences];
    let referencesForPremise = [...references, ...additionalPremiseReferences, ...additionalReferences];
    let referencesForConclusion = [...references, ...additionalReferences, ...additionalConclusionReferences];

    const actionHighlights = _.chain(state.highlighting.actionHighlights)
      .filter(actionHighlight => {
        const references = actionHighlight.action ? referencesForAction : referencesForPremise;
        return _.some(references, reference => reference.matches(actionHighlight.reference))
      })
      .map(actionHighlight => { return {path: actionHighlight.reference.innerPath || [], action: actionHighlight.action}})
      .value();
    const staticHighlights = _.chain(state.highlighting.staticHighlights)
      .filter(staticHighlight => _.some(referencesForConclusion, reference => reference.matches(staticHighlight)))
      .map(staticHighlight => {return {path: staticHighlight.innerPath || []}})
      .value();

    return {
      expression,
      actionHighlights,
      staticHighlights,
      boundVariableLists,
      wrapBoundVariable,
      className,
      expressionToCopy
    }
  }
)(class HighlightableExpression extends React.Component {
  shouldComponentUpdate(nextProps, nextState, nextContext) {
    return !_.isEqual(this.props, nextProps);
  }
  render() {
    const {expression, actionHighlights, staticHighlights, wrapBoundVariable, className, expressionToCopy} = this.props;
    const expressionElement = <CopiableExpression expression={expression}
                                                  actionHighlights={actionHighlights}
                                                  staticHighlights={staticHighlights}
                                                  wrapBoundVariable={wrapBoundVariable}
                                                  expressionToCopy={expressionToCopy}
                                                  parentRequiresBrackets={false}/>;
    return className ? <span className={className}>{expressionElement}</span> : expressionElement;
  }
});
