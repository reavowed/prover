import _ from "lodash";
import React, {useContext} from "react";
import styled from "styled-components";
import {
  DefinedExpression,
  matchTemplate,
  PropertyExpression, RelatedObjectExpression,
  StandalonePropertyExpression,
  TypeExpression,
  TypeQualifierExpression, Variable
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
  const result = [];
  _.forEach(actions, a => {
    if (_.startsWith(a.path, initialPath)) {
      result.push({...a, path: a.path.slice(initialPath.length)});
    } else if (_.startsWith(initialPath, a.path)) {
      result.push({...a, path: []});
    }
  });
  return result;
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

export function ExpressionComponent({expression, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets, wrapBoundVariable, path, entryContext, displayContext, splitConjunction}) {
  entryContext = entryContext || useContext(EntryContext);
  displayContext = displayContext || useContext(DisplayContext);
  wrapBoundVariable = wrapBoundVariable || ((name) => formatHtml(name));

  function renderExpression(expression, path, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets, splitConjunction = false) {

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
    function renderMatch(match, path) {
      if (match.type === "boundVariable") {
        return wrapBoundVariable(match.name, match.index, path.concat(match.pathWithinMatch));
      } else {
        return renderChildExpression(
          match.expression,
          match.pathWithinMatch,
          {
            additionalBoundVariables: match.boundVariablesWithinMatch,
            parentRequiresBrackets: true // Display shorthands currently default to requiring brackets
          });
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

    function renderChildrenOfTag() {
      if (!(displayContext && displayContext.disableShorthands)) {
        const {displayShorthand, matches} = matchDisplayShorthand(expression) || {};
        if (matches) {
          let renderedMatches = matches.map(m => renderMatch(m, path));
          let formatString = (parentRequiresBrackets && displayShorthand.requiresBrackets) ?
            "(" + displayShorthand.baseFormatString + ")" :
            displayShorthand.baseFormatString;
          return formatHtmlWithoutWrapping(formatString, s => replacePlaceholders(s, renderedMatches));
        }
        let {disambiguatorAdder, match} = matchDisambiguatorAdder(expression) || {};
        let innerPath = [];
        if (disambiguatorAdder) {
          while (match) {
            expression = match.expression;
            innerPath = [...innerPath, ...match.pathWithinMatch];
            const newMatchResult = matchDisambiguatorAdder(expression) || {};
            match = newMatchResult.match;
          }
          if (expression.formatForHtml && expression.disambiguator && expression.components.length === 0) {
            return renderFormattableExpressionWithDisambiguator(disambiguatorAdder.disambiguator);
          } else {
            const renderedInner = renderChildExpression(expression, innerPath);
            return [renderedInner, <sub>{disambiguatorAdder.disambiguator}</sub>];
          }
        }
      }

      function splitConjunctions(expression, path) {
        if (expression instanceof DefinedExpression && _.includes(expression.definition.attributes, "conjunction")) {
          return _.flatMap(expression.components, (e, i) => splitConjunctions(e, [...path, i]));
        } else {
          return [[expression, path]];
        }
      }

      if (splitConjunction && !(displayContext && displayContext.disableShorthands)) {
        return [joinAsList(splitConjunctions(expression, []).map(([e, p]) => renderChildExpression(e, p)))];
      }

      function renderQualifier(qualifier, components, subpath = [], sharedPaths = []) {
        const formattedComponents = _.map(components, (component, i) => renderChildExpression(component, [...subpath, i + 1]));
        return highlightSharedChild(formatHtmlWithoutWrapping(qualifier.format, s => replacePlaceholders(s, formattedComponents)), sharedPaths);
      }

      function renderFormattableExpressionWithDisambiguator(disambiguator, symbol) {
        const format = expression.formatForHtml(parentRequiresBrackets);
        const boundVariables = expression.boundVariableNames || [];
        const innerBoundVariables = boundVariables.length ? [...boundVariableLists, boundVariables] : boundVariableLists;
        const renderedBoundVariables = boundVariables.map((name, index) => wrapBoundVariable(name, index, path));
        const renderedComponents = expression.components.map((c, i) =>
          renderChildExpression(c, [i], {additionalBoundVariables: innerBoundVariables, parentRequiresBrackets: expression.definition ? expression.definition.requiresComponentBrackets : true})
        );
        const renderedSymbol = (disambiguator && !(displayContext?.disambiguators?.[expression.symbol]?.length <= 1)) ?
          <>{formatHtml(expression.symbol)}<sub>{disambiguator}</sub></> :
          formatHtml(expression.symbol);
        return formatHtmlWithoutWrapping(format, s => replacePlaceholders(s, [renderedSymbol, ...renderedBoundVariables, ...renderedComponents]));
      }

      function addBrackets(result) {
        if (parentRequiresBrackets) {
          result.unshift("(");
          result.push(")");
        }
        return result;
      }

      if (expression instanceof TypeExpression) {
        const hasExplicitQualifier = !!expression.explicitQualifier;
        function getPropertyPath(propertyIndex) {
          const depth = numberOfObjectsAndProperties - 1 - propertyIndex;
          return [..._.times(depth, _.constant(0)), 1];
        }
        function getObjectPath(objectIndex) {
          const depth = expression.objects.length - 1 - objectIndex;
          return [..._.times(depth, _.constant(0)), 1];
        }

        const numberOfObjectsAndProperties = expression.properties.length + expression.objects.length;
        const typePath = _.times(expression.properties.length + (hasExplicitQualifier ? 1 : 0), _.constant(0));
        const termPath = [...typePath, 0];
        const qualifierPath =  [..._.times(expression.properties.length, _.constant(0)), 1];
        const propertyPaths = _.map(_.range(expression.properties.length), getPropertyPath);
        const objectPaths = _.map(_.range(expression.objects.length), getObjectPath);

        const sharedTermHighlightPaths = [typePath, ...(hasExplicitQualifier ? [qualifierPath] : []), ...propertyPaths, ...objectPaths];
        const formattedTerm = renderChildExpression(
          expression.term,
          termPath,
          {
            additionalActionHighlights: filterPathsMultiple(actionHighlights, sharedTermHighlightPaths),
            additionalStaticHighlights: filterPathsMultiple(staticHighlights, sharedTermHighlightPaths)
          });
        const formattedIs = highlightSharedChild("is", sharedTermHighlightPaths);

        const articleWord = expression.properties.length ? expression.properties[0].name : expression.definition.name;
        const article = _.includes("aeiou", articleWord[0]) ? "an" : "a";
        const formattedArticle = highlightSharedChild(article, [typePath]);
        const formattedName = highlightSharedChild(expression.definition.name, [typePath]);

        const formattedProperties = _.flatMap(expression.properties, (p, i) => {
          const formattedProperty = highlightChild(p.name, getPropertyPath(i));
          if (i === 0)
            return [formattedProperty];
          else
            return [<>, </>, formattedProperty];
        });

        const result = [formattedTerm, <> </>, formattedIs, <> </>, formattedArticle, <> </>, ...formattedProperties, <> </>, formattedName];
        if (expression.explicitQualifier) {
          const propertyIndexesRequiringQualifier = _.chain(expression.properties).map((p, i) => (p.requiredParentQualifier === expression.explicitQualifier.symbol) ? i : null).filter(x => x !== null).value();
          const objectIndexesRequiringQualifier = _.chain(expression.objects).map(([d, t], i) => (d.requiredParentQualifier === expression.explicitQualifier.symbol) ? i : null).filter(x => x !== null).value();
          const qualifierHighlightPaths = [qualifierPath, ...propertyIndexesRequiringQualifier.map(getPropertyPath), objectIndexesRequiringQualifier.map(getObjectPath)];
          const formattedQualifier = renderQualifier(expression.explicitQualifier.qualifier, expression.qualifierComponents, qualifierPath, qualifierHighlightPaths);
          result.push(<> </>);
          result.push(formattedQualifier);
        } else if (expression.definition.defaultQualifier) {
          const qualifierHighlightPaths = [typePath, ...propertyPaths, ...objectPaths];
          const formattedQualifier = renderQualifier(expression.definition.defaultQualifier, expression.qualifierComponents, typePath, qualifierHighlightPaths);
          result.push(<> </>);
          result.push(formattedQualifier);
        }
        if (expression.objects.length) {
          const formattedObjects = _.map(expression.objects, ([objectDefinition, objectTerm], index) => {
            const termPath = [...getObjectPath(index), 0];
            const renderedTerm = renderChildExpression(objectTerm, termPath);
            return highlightSharedChild(<>{objectDefinition.name} {renderedTerm}</>, getObjectPath(index))
          });
          result.push(<> </>);
          result.push(highlightSharedChild("with", _.range(expression.objects.length).map(getObjectPath)));
          result.push(<> </>);
          result.push(joinAsList(formattedObjects));
        }
        return addBrackets(result);
      } else if (expression instanceof TypeQualifierExpression) {
        const formattedTerm = renderChildExpression(expression.term, [...path, 0]);
        const formattedQualifier = renderQualifier(expression.definition.qualifier, expression.qualifierComponents);
        return addBrackets([formattedTerm, <> is </>, formattedQualifier]);
      } else if (expression instanceof PropertyExpression) {
        const formattedTerm = renderChildExpression(expression.term, [...path, 0]);
        const result = [formattedTerm, <> is </>, expression.definition.name];
        const qualifier = _.find(expression.typeDefinition.qualifiers, q => q.symbol === expression.definition.requiredParentQualifier)?.qualifier || expression.typeDefinition.defaultQualifier;
        if (qualifier) {
          const formattedQualifier = renderQualifier(qualifier, expression.qualifierComponents);
          result.push(<> </>);
          result.push(formattedQualifier);
        }
        return addBrackets(result);
      } else if (expression instanceof RelatedObjectExpression) {
        const formattedTerm = renderChildExpression(expression.term, [...path, 0]);
        const formattedParentTerm = renderChildExpression(expression.parentTerm, [...path, 1]);
        const result = [formattedTerm, <> is {expression.definition.article} {expression.definition.name} for </>, formattedParentTerm];
        const qualifier = _.find(expression.typeDefinition.qualifiers, q => q.symbol === expression.definition.requiredParentQualifier)?.qualifier || expression.typeDefinition.defaultQualifier;
        if (qualifier) {
          const formattedQualifier = renderQualifier(qualifier, expression.qualifierComponents);
          result.push(<> </>);
          result.push(formattedQualifier);
        }
        return addBrackets(result);
      } else if (expression instanceof StandalonePropertyExpression) {
        const formattedTerm = renderChildExpression(expression.term, [...path, 0]);
        return addBrackets([formattedTerm, <> is </>, expression.definition.name]);
      } else if (expression instanceof Variable) {
        if (displayContext && displayContext.variableDefinitions) {
          const statementVariableRegex = /^s(\d+)$/;
          const termVariableRegex = /^t(\d+)$/;
          const statementMatch = expression.name.match(statementVariableRegex);
          const termMatch = expression.name.match(termVariableRegex);
          const name = statementMatch ? displayContext.variableDefinitions.statements[parseInt(statementMatch[1])].name :
            termMatch ? displayContext.variableDefinitions.terms[parseInt(termMatch[1])].name :
            throw new Error("Unrecognised variable " + expression.name);
          const result = formatHtmlWithoutWrapping(name);
          if (expression.components.length > 0) {
            result.push("(");
            result.push(renderChildExpression(expression.components[0], [0]));
            for (let i = 1; i < expression.components.length; ++i) {
              result.push(", ");
              result.push(renderChildExpression(expression.components[i], [i]));
            }
            result.push(")");
          }
          return result;
        } else {
          return formatHtmlWithoutWrapping(expression.name);
        }
      } else if (expression.formatForHtml) {
        return renderFormattableExpressionWithDisambiguator(expression.disambiguator);
      } else if (expression.textForHtml) {
        return formatHtmlWithoutWrapping(expression.textForHtml(boundVariableLists));
      } else {
        throw "Could not render expression " + expression;
      }
    }

    function highlightChild(child, subpath) {
      return highlight(child, filterPaths(actionHighlights, subpath), filterPaths(staticHighlights, subpath))
    }
    function highlightSharedChild(child, subpaths) {
      return highlight(child, filterPathsMultiple(actionHighlights, subpaths), filterPathsMultiple(staticHighlights, subpaths))
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

    function renderChildExpression(expression, subpath, {additionalBoundVariables = [], parentRequiresBrackets = false, additionalActionHighlights = [], additionalStaticHighlights = []} = {}) {
      return renderExpression(
        expression,
        [...path, ...subpath],
        [...filterPaths(actionHighlights, subpath), ...additionalActionHighlights],
        [...filterPaths(staticHighlights, subpath), ...additionalStaticHighlights],
        [...boundVariableLists, ...additionalBoundVariables],
        parentRequiresBrackets,
        false);
    }

    return highlight(
      renderChildrenOfTag().map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>),
      actionHighlights,
      staticHighlights);
  }
  path = path || [];
  boundVariableLists = boundVariableLists || [];
  actionHighlights = actionHighlights || [];
  staticHighlights = staticHighlights || [];

  return renderExpression(expression, path, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets, splitConjunction);
}

export const CopiableExpression = (props) => {
  const expressionToCopy = props.expressionToCopy || props.expression;
  const boundVariableLists = props.boundVariableLists || useContext(BoundVariableLists) || [];
  const {expression, ...otherProps} = props;

  return <span onContextMenu={() => navigator.clipboard.writeText(expressionToCopy.serializeNicely(boundVariableLists))}>
    <ExpressionComponent expression={expression} {...otherProps} boundVariableLists={boundVariableLists}/>
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
