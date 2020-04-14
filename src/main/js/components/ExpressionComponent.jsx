import _ from "lodash";
import React, {useContext} from "react";
import styled from "styled-components";
import {matchTemplate, PropertyExpression, StandalonePropertyExpression, TypeExpression} from "../models/Expression";
import EntryContext from "./EntryContext";
import {formatHtml, formatHtmlWithoutWrapping, replacePlaceholders} from "./helpers/Formatter";
import BoundVariableLists from "./pages/theorem/steps/BoundVariableLists";
import ProofContext from "./pages/theorem/ProofContext";
import TheoremContext from "./pages/theorem/TheoremContext";

const HighlightingStyle = styled.span`
  color: ${props => props.isPremise ? "red" : props.isConclusion ? "blue" : null};
  cursor: ${props => props.isClickable && "pointer"};
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
  const theoremContext = useContext(TheoremContext);
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
        [...boundVariableLists, ...match.boundVariablesWithinMatch],
        true // Display shorthands currently default to requiring brackets
      );
    }
  }
  function matchDisambiguatorAdder(expression) {
    for (const disambiguatorAdder of _.reverse(entryContext.disambiguatorAdders.slice())) {
      const matches = matchTemplate(disambiguatorAdder.template, expression, [], []);
      if (matches && matches.length === 1) {
        return {disambiguatorAdder, match: matches[0]};
      }
    }
  }

  function renderChildrenOfTag(expression, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable, parentRequiresBrackets) {
    if (!(theoremContext && theoremContext.disableShorthands)) {
      const {displayShorthand, matches} = matchDisplayShorthand(expression) || {};
      if (matches) {
        let renderedMatches = matches.map(m => renderMatch(m, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable));
        let formatString = (parentRequiresBrackets && displayShorthand.requiresBrackets) ?
          "(" + displayShorthand.baseFormatString + ")" :
          displayShorthand.baseFormatString;
        return formatHtmlWithoutWrapping(formatString, s => replacePlaceholders(s, renderedMatches));
      }
    }

    let {disambiguatorAdder, match} = matchDisambiguatorAdder(expression) || {};
    if (disambiguatorAdder) {
      while (match) {
        expression = match.expression;
        path = [...path, ...match.pathWithinMatch];
        actionHighlights = filterPaths(actionHighlights, path);
        staticHighlights = filterPaths(staticHighlights, path);
        const newMatchResult = matchDisambiguatorAdder(expression) || {};
        match = newMatchResult.match;
      }
      if (expression.formatForHtml && expression.disambiguator && expression.components.length === 0) {
        return renderFormattableExpressionWithDisambiguator(disambiguatorAdder.disambiguator);
      } else {
        const renderedInner = renderExpression(expression, path, actionHighlights, staticHighlights, boundVariableLists, true);
        return [renderedInner, <sub>{disambiguatorAdder.disambiguator}</sub>];
      }
    }
    return renderNormally();


    function renderComponentExpression(components, formatString, parentPath, actionHighlights, staticHighlights, sharedActionHighlights, sharedStaticHighlights) {
      const formattedComponents = _.map(components, (component, i) => {
        const componentPath = [...parentPath, i + 1];
        return renderExpression(component, componentPath, [...sharedActionHighlights, ...filterPaths(actionHighlights, componentPath)], [...sharedStaticHighlights, ...filterPaths(staticHighlights, componentPath)], boundVariableLists, false);
      });
      return formatHtmlWithoutWrapping(formatString, s => replacePlaceholders(s, formattedComponents));
    }
    function renderFormattableExpressionWithDisambiguator(disambiguator) {
      const format = expression.formatForHtml(parentRequiresBrackets);
      const boundVariables = expression.boundVariableNames || [];
      const innerBoundVariables = boundVariables.length ? [...boundVariableLists, boundVariables] : boundVariableLists;
      const renderedBoundVariables = boundVariables.map((name, index) => wrapBoundVariable(name, index, path));
      const renderedComponents = expression.components.map((c, i) =>
        renderExpression(c, [...path, i], filterPaths(actionHighlights, [i]), filterPaths(staticHighlights, [i]), innerBoundVariables, expression.definition ? expression.definition.requiresComponentBrackets : true)
      );
      const renderedSymbol = disambiguator ?
        <>{formatHtml(expression.symbol)}<sub>{disambiguator}</sub></> :
        formatHtml(expression.symbol);
      return formatHtmlWithoutWrapping(format, s => replacePlaceholders(s, [...renderedBoundVariables, ...renderedComponents, renderedSymbol]));
    }

    function renderNormally() {
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
        const articleWord = expression.properties.length ? expression.properties[0].name : expression.definition.name;
        const article = _.includes("aeiou", articleWord[0]) ? "an" : "a";
        const formattedArticle = renderExpression({textForHtml: () => article}, [], typeActionHighlights, typeStaticHighlights, boundVariableLists, false);
        const formattedName = renderExpression({textForHtml: () => expression.definition.name}, [], typeActionHighlights, typeStaticHighlights, boundVariableLists, false);

        const formattedComponentText = renderComponentExpression(expression.otherComponents, expression.definition.componentFormatString, typePath, actionHighlights, staticHighlights, sharedActionHighlights, sharedStaticHighlights);

        const formattedProperties = _.flatMap(expression.properties, (p, i) => {
          const formattedProperty = renderExpression({textForHtml: () => p.name}, [], filterPaths(actionHighlights, getPropertyPath(i)), filterPaths(staticHighlights, getPropertyPath(i)), boundVariableLists, false);
          if (i === 0)
            return [formattedProperty];
          else
            return [<>, </>, formattedProperty];
        });
        return [formattedTerm, <> </>, formattedIs, <> </>, formattedArticle, <> </>, ...formattedProperties, <> </>, formattedName, <> </>, formattedComponentText];
      } else if (expression instanceof PropertyExpression) {
        const formattedTerm = renderExpression(expression.term, [...path, 0], filterPaths(actionHighlights, [...path, 0]), filterPaths(staticHighlights, [...path, 0]), boundVariableLists, false);
        const formattedComponentText = renderComponentExpression(expression.otherComponents, expression.typeDefinition.componentFormatString, path, actionHighlights, staticHighlights, [], []);
        return [formattedTerm, <> is </>, expression.definition.name, <> </>, formattedComponentText];
      } else if (expression instanceof StandalonePropertyExpression) {
        const formattedTerm = renderExpression(expression.term, [...path, 0], filterPaths(actionHighlights, [...path, 0]), filterPaths(staticHighlights, [...path, 0]), boundVariableLists, false);
        if (expression.definition.numberOfComponents.length > 0) {
          const formattedComponentText = renderComponentExpression(expression.otherComponents, expression.typeDefinition.componentFormatString, path, actionHighlights, staticHighlights, [], []);
          return [formattedTerm, <> is </>, expression.definition.name, <> </>, formattedComponentText];
        }
        return [formattedTerm, <> is </>, expression.definition.name];
      } else if (expression.formatForHtml) {
        return renderFormattableExpressionWithDisambiguator(expression.disambiguator);
      } else if (expression.textForHtml) {
        return formatHtmlWithoutWrapping(expression.textForHtml(boundVariableLists));
      } else {
        throw "Could not render expression " + expression;
      }
    }
  }

  function renderExpression(expression, path, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets) {
    const matchingActionHighlight = _.find(actionHighlights, p => p.path.length === 0);
    const shouldStaticHighlight = _.some(staticHighlights, p => p.path.length === 0);

    const highlightingProps = {
      isConclusion: !!shouldStaticHighlight,
      isPremise: !shouldStaticHighlight && !!matchingActionHighlight,
      isClickable: !!(matchingActionHighlight && matchingActionHighlight.action)
    };

    const props = {};
    if (!shouldStaticHighlight && matchingActionHighlight && matchingActionHighlight.action) {
      props.onClick = (e) => {
        matchingActionHighlight.action(expression.serialize());
        e.stopPropagation();
      }
    }
    return <HighlightingStyle {...highlightingProps}>
      {renderChildrenOfTag(expression, path, actionHighlights || [], staticHighlights || [], boundVariableLists || [], wrapBoundVariable, parentRequiresBrackets).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>)}
    </HighlightingStyle>;
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
    let {expression, references, additionalReferences, additionalPremiseReferences, additionalConclusionReferences, wrapBoundVariable, className, expressionToCopy, path} = this.props;
    path = path || [];
    additionalReferences = additionalReferences || [];
    additionalPremiseReferences = additionalPremiseReferences || [];
    additionalConclusionReferences = additionalConclusionReferences || [];
    let referencesForAction = [...references, ...additionalPremiseReferences];
    let referencesForPremise = [...references, ...additionalPremiseReferences, ...additionalReferences];
    let referencesForConclusion = [...references, ...additionalReferences, ...additionalConclusionReferences];

    function renderFromContext(context) {
      const [allActionHighlights, allStaticHighlights] = context.getHighlighting();
      let actionHighlights = _.chain(allActionHighlights)
        .filter(actionHighlight => {
          const references = actionHighlight.action ? referencesForAction : referencesForPremise;
          return _.some(references, reference => reference.matches(actionHighlight.reference))
        })
        .map(actionHighlight => {
          return {path: actionHighlight.reference.innerPath || [], action: actionHighlight.action}
        })
        .filter(highlight => _.startsWith(highlight.path, path) || _.startsWith(path, highlight.path))
        .map(highlight => {
          highlight.path = highlight.path.slice(path.length);
          return highlight;
        })
        .value();
      let staticHighlights = _.chain(allStaticHighlights)
        .filter(staticHighlight => _.some(referencesForConclusion, reference => reference.matches(staticHighlight)))
        .map(staticHighlight => {
          return {path: staticHighlight.innerPath || []}
        })
        .filter(highlight => _.startsWith(highlight.path, path) || _.startsWith(path, highlight.path))
        .map(highlight => {
          highlight.path = highlight.path.slice(path.length);
          return highlight;
        })
        .value();
      const expressionElement = <CopiableExpression expression={expression}
                                                    actionHighlights={actionHighlights}
                                                    staticHighlights={staticHighlights}
                                                    wrapBoundVariable={wrapBoundVariable}
                                                    expressionToCopy={expressionToCopy}
                                                    path={path}
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
