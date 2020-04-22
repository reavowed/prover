import _ from "lodash";
import React, {useContext} from "react";
import {act} from "react-dom/test-utils";
import styled from "styled-components";
import {
  DefinedExpression,
  matchTemplate,
  PropertyExpression,
  StandalonePropertyExpression,
  TypeExpression,
  TypeQualifierExpression
} from "../models/Expression";
import DisplayContext from "./DisplayContext";
import EntryContext from "./EntryContext";
import {formatHtml, formatHtmlWithoutWrapping, replacePlaceholders} from "./helpers/Formatter";
import {joinAsList} from "./helpers/reactFunctions";
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
      if (_.startsWith(initialPath, action.path)) {
        result.push({...action, path: []});
        break;
      }
    }
  }
  return result;
}

export function ExpressionComponent({expression, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets, wrapBoundVariable, path, entryContext}) {
  entryContext = entryContext || useContext(EntryContext);
  const displayContext = useContext(DisplayContext);
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
    if (!(displayContext && displayContext.disableShorthands)) {
      const {displayShorthand, matches} = matchDisplayShorthand(expression) || {};
      if (matches) {
        let renderedMatches = matches.map(m => renderMatch(m, path, actionHighlights, staticHighlights, boundVariableLists, wrapBoundVariable));
        let formatString = (parentRequiresBrackets && displayShorthand.requiresBrackets) ?
          "(" + displayShorthand.baseFormatString + ")" :
          displayShorthand.baseFormatString;
        return formatHtmlWithoutWrapping(formatString, s => replacePlaceholders(s, renderedMatches));
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
    }

    return renderNormally();

    function renderQualifier(qualifier, components, parentPath, actionHighlights, staticHighlights, sharedPaths) {
      sharedPaths = sharedPaths || [];
      const formattedComponents = _.map(components, (component, i) => {
        const componentPath = [...parentPath, i + 1];
        return renderExpression(component, componentPath, filterPaths(actionHighlights, componentPath), filterPaths(staticHighlights, componentPath), boundVariableLists, false);
      });
      return highlight(
        formatHtmlWithoutWrapping(qualifier.format, s => replacePlaceholders(s, formattedComponents)),
        filterPathsMultiple(actionHighlights, [parentPath, ...sharedPaths]),
        filterPathsMultiple(staticHighlights, [parentPath, ...sharedPaths]));
    }

    function renderFormattableExpressionWithDisambiguator(disambiguator) {
      const format = expression.formatForHtml(parentRequiresBrackets);
      const boundVariables = expression.boundVariableNames || [];
      const innerBoundVariables = boundVariables.length ? [...boundVariableLists, boundVariables] : boundVariableLists;
      const renderedBoundVariables = boundVariables.map((name, index) => wrapBoundVariable(name, index, path));
      const renderedComponents = expression.components.map((c, i) =>
        renderExpression(c, [...path, i], filterPaths(actionHighlights, [i]), filterPaths(staticHighlights, [i]), innerBoundVariables, expression.definition ? expression.definition.requiresComponentBrackets : true)
      );
      const renderedSymbol = (disambiguator && !(displayContext?.disambiguators?.[expression.symbol]?.length <= 1)) ?
        <>{formatHtml(expression.symbol)}<sub>{disambiguator}</sub></> :
        formatHtml(expression.symbol);
      return formatHtmlWithoutWrapping(format, s => replacePlaceholders(s, [renderedSymbol, ...renderedBoundVariables, ...renderedComponents]));
    }

    function renderNormally() {
      if (expression instanceof TypeExpression) {
        const hasExplicitQualifier = !!expression.explicitQualifier;
        function getPropertyPath(propertyIndex) {
          const depth = expression.properties.length - 1 - propertyIndex;
          return [..._.times(depth, _.constant(0)), 1];
        }

        const typePath = _.times(expression.properties.length + (hasExplicitQualifier ? 1 : 0), _.constant(0));
        const termPath = [...typePath, 0];
        const qualifierPath =  [..._.times(expression.properties.length, _.constant(0)), 1];
        const propertyPaths = _.map(_.range(expression.properties.length), getPropertyPath);

        const sharedTermHighlightPaths = [typePath, ...(hasExplicitQualifier ? [qualifierPath] : []), ...propertyPaths];
        const formattedTerm = renderExpression(
          expression.term,
          termPath,
          [...filterPaths(actionHighlights, termPath), ...filterPathsMultiple(actionHighlights, sharedTermHighlightPaths)],
          [...filterPaths(staticHighlights, termPath), ...filterPathsMultiple(staticHighlights, sharedTermHighlightPaths)],
          boundVariableLists,
          false);
        const formattedIs = highlight("is", filterPathsMultiple(actionHighlights, sharedTermHighlightPaths), filterPathsMultiple(staticHighlights, sharedTermHighlightPaths));

        const articleWord = expression.properties.length ? expression.properties[0].name : expression.definition.name;
        const article = _.includes("aeiou", articleWord[0]) ? "an" : "a";
        const formattedArticle = highlight(article, filterPathsMultiple(actionHighlights, [typePath]), filterPathsMultiple(staticHighlights, [typePath]));
        const formattedName = highlight(expression.definition.name, filterPathsMultiple(actionHighlights, [typePath]), filterPathsMultiple(staticHighlights, [typePath]));

        const formattedProperties = _.flatMap(expression.properties, (p, i) => {
          const formattedProperty = highlight(p.name, filterPaths(actionHighlights, getPropertyPath(i)), filterPaths(staticHighlights, getPropertyPath(i)));
          if (i === 0)
            return [formattedProperty];
          else
            return [<>, </>, formattedProperty];
        });

        const result =  [formattedTerm, <> </>, formattedIs, <> </>, formattedArticle, <> </>, ...formattedProperties, <> </>, formattedName];
        if (expression.explicitQualifier) {
          const propertyIndexesRequiringQualifier = _.chain(expression.properties).map((p, i) => (p.requiredParentQualifier === expression.explicitQualifier.symbol) ? i : null).filter(x => x !== null).value();
          const qualifierHighlightPaths = propertyIndexesRequiringQualifier.map(getPropertyPath);
          const formattedQualifier = renderQualifier(expression.explicitQualifier.qualifier, expression.qualifierComponents, qualifierPath, actionHighlights, staticHighlights, qualifierHighlightPaths);
          result.push(<> </>);
          result.push(formattedQualifier);
        } else if (expression.definition.defaultQualifier) {
          const qualifierHighlightPaths = [typePath, ...propertyPaths];
          const formattedQualifier = renderQualifier(expression.definition.defaultQualifier, expression.qualifierComponents, typePath, actionHighlights, staticHighlights, qualifierHighlightPaths);
          result.push(<> </>);
          result.push(formattedQualifier);
        }
        if (parentRequiresBrackets) {
          result.unshift("(");
          result.push(")");
        }
        return result;
      } else if (expression instanceof TypeQualifierExpression) {
        const formattedTerm = renderExpression(expression.term, [...path, 0], filterPaths(actionHighlights, [...path, 0]), filterPaths(staticHighlights, [...path, 0]), boundVariableLists, false);
        const formattedQualifier = renderQualifier(expression.definition.qualifier, expression.qualifierComponents, path, actionHighlights, staticHighlights);
        return [formattedTerm, <> is </>, formattedQualifier];
      } else if (expression instanceof PropertyExpression) {
        const formattedTerm = renderExpression(expression.term, [...path, 0], filterPaths(actionHighlights, [...path, 0]), filterPaths(staticHighlights, [...path, 0]), boundVariableLists, false);
        const result = [formattedTerm, <> is </>, expression.definition.name];
        if (expression.typeDefinition.defaultQualifier) {
          const formattedQualifier = renderQualifier(expression.typeDefinition.defaultQualifier, expression.qualifierComponents, path, actionHighlights, staticHighlights);
          result.push(<> </>);
          result.push(formattedQualifier);
        }
        return result;
      } else if (expression instanceof StandalonePropertyExpression) {
        const formattedTerm = renderExpression(expression.term, [...path, 0], filterPaths(actionHighlights, [...path, 0]), filterPaths(staticHighlights, [...path, 0]), boundVariableLists, false);
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

  function highlight(children, actionHighlights, staticHighlights) {
    const matchingActionHighlight = _.find(actionHighlights, p => p.path.length === 0);
    const shouldStaticHighlight = _.some(staticHighlights, p => p.path.length === 0);
    const props = {
      isConclusion: !!shouldStaticHighlight,
      isPremise: !shouldStaticHighlight && !!matchingActionHighlight,
      isClickable: !!(matchingActionHighlight && matchingActionHighlight.action)
    };
    if (!shouldStaticHighlight && matchingActionHighlight && matchingActionHighlight.action) {
      props.onClick = (e) => {
        matchingActionHighlight.action(expression.serialize());
        e.stopPropagation();
      }
    }
    return <HighlightingStyle {...props}>
      {children}
    </HighlightingStyle>;
  }

  function renderExpression(expression, path, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets) {
    return highlight(
      renderChildrenOfTag(expression, path, actionHighlights || [], staticHighlights || [], boundVariableLists || [], wrapBoundVariable, parentRequiresBrackets).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>),
      actionHighlights,
      staticHighlights);
  }
  path = path || [];

  return renderExpression(expression, path, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets);
}

export const CopiableExpression = (props) => {
  const displayContext = useContext(DisplayContext);
  const expressionToCopy = props.expressionToCopy || props.expression;
  const boundVariableLists = props.boundVariableLists || useContext(BoundVariableLists) || [];
  const {expression, ...otherProps} = props;

  function splitConjunctions(expression) {
    if (expression instanceof DefinedExpression && _.includes(expression.definition.attributes, "conjunction")) {
      return _.flatMap(expression.components, splitConjunctions);
    } else {
      return [expression];
    }
  }
  function toComponent(expression) {
    return <ExpressionComponent expression={expression} {...otherProps} boundVariableLists={boundVariableLists}/>
  }

  const component = (props.splitConjunction && !(displayContext && displayContext.disableShorthands)) ?
    joinAsList(splitConjunctions(expression).map(toComponent)) :
    toComponent(expression);

  return <span onContextMenu={() => navigator.clipboard.writeText(expressionToCopy.serializeNicely(boundVariableLists))}>
    {component}
  </span>;
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
