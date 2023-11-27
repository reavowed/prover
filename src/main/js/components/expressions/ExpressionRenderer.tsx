import {
  BoundVariableMatchResult,
  DefinedExpression,
  Expression,
  ExpressionMatchResult, FunctionParameter,
  MatchResult,
  PropertyExpression,
  RelatedObjectExpression,
  StandalonePropertyExpression,
  TypeExpression,
  TypeQualifierExpression,
  TypeRelationExpression,
  Variable
} from "../../models/Expression";
import {AvailableEntries} from "../AvailableEntriesContext";
import DisplaySettings from "../DisplaySettings";
import React from "react";
import _ from "lodash";
import ExpressionHighlighting, {ExpressionHighlightingProps} from "./ExpressionHighlighting";
import {formatHtml, formatHtmlWithoutWrapping, replacePlaceholders} from "../helpers/Formatter";
import {joinAsList} from "../helpers/reactFunctions";
import {matchDisambiguatorAdder, matchDisplayShorthand} from "./ShorthandMatching";
import {QualifierDefinition} from "../definitions/DefinitionParts";
import {CompoundReactNode, isDefined, SimpleReactNode, startsWith} from "../../utils";

export type RenderExpressionOptions<T extends Expression | string> = {
  expression: T
  path: number[]
  actionHighlights: ExpressionActionHighlight[]
  staticHighlights: ExpressionStaticHighlight[]
  boundVariableLists: string[][]
  parentRequiresBrackets: boolean
  splitConjunction: boolean
  availableEntries: AvailableEntries
  displaySettings: DisplaySettings
  wrapBoundVariable: (name: string, index: number, path: number[]) => SimpleReactNode
}
type ChildRenderOptions = {
  additionalBoundVariables?: string[][],
  parentRequiresBrackets?: boolean,
  additionalActionHighlights?: ExpressionActionHighlight[],
  additionalStaticHighlights?: ExpressionStaticHighlight[]
}

export type ExpressionActionHighlight = {
  path: number[]
  action?: () => void
}
export type ExpressionStaticHighlight = {
  path: number[]
}
function isBoundVariable(match: MatchResult): match is BoundVariableMatchResult {
  return match.type === "boundVariable"
}
function isFormattable(expression: Expression): expression is Variable | DefinedExpression {
  return "formatForHtml" in expression;
}
function filterPaths<T extends {path: number[]}>(actions: T[], initialPath: number[]): T[] {
  const result: T[] = [];
  _.forEach(actions, a => {
    if (startsWith(a.path, initialPath)) {
      result.push({...a, path: a.path.slice(initialPath.length)});
    } else if (startsWith(initialPath, a.path)) {
      result.push({...a, path: []});
    }
  })
  return result;
}

function filterPathsMultiple<T extends {path: number[]}>(actions: T[], initialPaths: number[][]) {
  const result = [];
  for(let i = 0; i < actions.length; ++i) {
    const action = actions[i];
    for(let j = 0; j < initialPaths.length; ++j) {
      const initialPath = initialPaths[j];
      if (startsWith(initialPath, action.path)) {
        result.push({...action, path: []});
        break;
      }
    }
  }
  return result;
}

function addBrackets(elements: SimpleReactNode[], props: RenderExpressionOptions<Expression>): SimpleReactNode[] {
  if (props.parentRequiresBrackets) {
    return ["(", ...elements, ")"]
  } else {
    return elements;
  }
}

export default {
  renderExpression(props: RenderExpressionOptions<Expression | string>): React.ReactElement {
    return this.highlight(
      this.renderExpressionWithoutHighlighting(props),
      props.actionHighlights,
      props.staticHighlights);
  },
  renderExpressionWithoutHighlighting(props: RenderExpressionOptions<Expression | string>) {
    if (_.isString(props.expression)) {
      return formatHtmlWithoutWrapping(props.expression);
    }
    return this.renderShorthand(props as RenderExpressionOptions<Expression>) ||
      this.renderSplitConjunction(props as RenderExpressionOptions<Expression>) ||
      this.renderDirectly(props as RenderExpressionOptions<Expression>);
  },
  renderChildExpression(childExpression: Expression, childPath: number[], parentProps: RenderExpressionOptions<Expression>, renderOptions: ChildRenderOptions = {}): SimpleReactNode {
    return this.renderExpression({
      ...parentProps,
      expression: childExpression,
      path: [...parentProps.path, ...childPath],
      actionHighlights: [...filterPaths(parentProps.actionHighlights, childPath), ...(renderOptions.additionalActionHighlights || [])],
      staticHighlights: [...filterPaths(parentProps.staticHighlights, childPath), ...(renderOptions.additionalStaticHighlights || [])],
      boundVariableLists: [...parentProps.boundVariableLists, ...(renderOptions.additionalBoundVariables || [])],
      parentRequiresBrackets: renderOptions.parentRequiresBrackets || false,
      splitConjunction: false
    });
  },
  renderShorthand(props: RenderExpressionOptions<Expression>): SimpleReactNode[] | undefined {
    if (!props.displaySettings.disableShorthands) {
      return this.renderDisplayShorthand(props) || this.renderDisambiguatorAdder(props);
    }
  },
  renderDisplayShorthand(props: RenderExpressionOptions<Expression>): SimpleReactNode[] | undefined {
    const displayShorthandMatchResult = matchDisplayShorthand(props.expression, props.availableEntries, props.displaySettings);
    if (displayShorthandMatchResult) {
      const {matches, displayShorthand} = displayShorthandMatchResult;
      let renderedMatches = matches.map(m => this.renderExpressionMatch(m, props));
      let formatString = (props.parentRequiresBrackets && displayShorthand.requiresBrackets) ?
        "(" + displayShorthand.baseFormatString + ")" :
        displayShorthand.baseFormatString;
      return formatHtmlWithoutWrapping(formatString, s => replacePlaceholders(s, renderedMatches));
    }
  },
  renderDisambiguatorAdder(props: RenderExpressionOptions<Expression>): SimpleReactNode[] | undefined {
      let disambiguatorAdderMatchResult = matchDisambiguatorAdder(props.expression, props.availableEntries);
      if (disambiguatorAdderMatchResult) {
        const disambiguatorAdder = disambiguatorAdderMatchResult.disambiguatorAdder;
        let innerPath: number[] = [];
        let expression = props.expression;
        while (disambiguatorAdderMatchResult) {
          let match = disambiguatorAdderMatchResult.match as ExpressionMatchResult;
          expression = match.expression;
          innerPath = [...innerPath, ...match.pathWithinMatch];
          disambiguatorAdderMatchResult = matchDisambiguatorAdder(expression, props.availableEntries);
        }
        if (isFormattable(expression) && expression.disambiguator && expression.components.length === 0) {
          return this.renderFormattableExpressionWithDisambiguator(disambiguatorAdder.disambiguator, props as RenderExpressionOptions<Variable | DefinedExpression>);
        } else {
          const renderedInner = this.renderChildExpression(expression, innerPath, props);
          return [renderedInner, <sub>{disambiguatorAdder.disambiguator}</sub>];
        }
      }
  },
  renderFormattableExpressionWithDisambiguator(disambiguator: string | null | undefined, props: RenderExpressionOptions<Variable | DefinedExpression>): SimpleReactNode[] {
    const {expression, path, parentRequiresBrackets, boundVariableLists, wrapBoundVariable, displaySettings} = props;
    const format = expression.formatForHtml(parentRequiresBrackets);
    const boundVariables = expression instanceof DefinedExpression ? expression.boundVariableNames : [];
    const innerBoundVariables = boundVariables.length ? [...boundVariableLists, boundVariables] : boundVariableLists;
    const renderedBoundVariables = boundVariables.map((name, index) => wrapBoundVariable(name, index, path));
    const renderedComponents = expression.components.map((c, i) =>
      this.renderChildExpression(c, [i], props, {additionalBoundVariables: innerBoundVariables, parentRequiresBrackets: expression instanceof DefinedExpression ? expression.definition.requiresComponentBrackets : true})
    );
    const renderedSymbol = (disambiguator && !(displaySettings?.disambiguators?.[expression.symbol]?.length <= 1)) ?
      <>{formatHtml(expression.symbol)}<sub>{disambiguator}</sub></> :
      formatHtmlWithoutWrapping(expression.symbol);
    return formatHtmlWithoutWrapping(format, s => replacePlaceholders(s, [renderedSymbol, ...renderedBoundVariables, ...renderedComponents]));
  },
  renderExpressionMatch(match: MatchResult, parentProps: RenderExpressionOptions<Expression>) {
    if (isBoundVariable(match)) {
      return parentProps.wrapBoundVariable(match.name, match.index, parentProps.path.concat(match.pathWithinMatch));
    } else {
      return this.renderChildExpression(
        match.expression,
        match.pathWithinMatch,
        parentProps,
        {
          additionalBoundVariables: match.boundVariablesWithinMatch,
          parentRequiresBrackets: true // Display shorthands currently default to requiring brackets
        });
    }
  },
  renderSplitConjunction(props: RenderExpressionOptions<Expression>): SimpleReactNode[] | undefined {
    if (!props.splitConjunction || !props.displaySettings.disableShorthands) {
      return undefined;
    }
    function splitConjunctions(expression: Expression, childPath: number[]): {expression: Expression, childPath: number[]}[] {
      if (expression instanceof DefinedExpression && _.includes(expression.definition.attributes, "conjunction")) {
        return _.flatMap(expression.components, (e, i) => splitConjunctions(e, [...childPath, i]));
      } else {
        return [{expression, childPath}]
      }
    }
    const splitExpression = splitConjunctions(props.expression, []);
    if (splitExpression.length > 1) {
      return joinAsList(
        splitExpression.map(({expression, childPath}) =>
          this.renderChildExpression(expression, childPath, props)
        )
      )
    }
  },
  renderDirectly(props: RenderExpressionOptions<Expression>): SimpleReactNode[] {
    if (props.expression instanceof Variable) {
      return this.renderVariable(props as RenderExpressionOptions<Variable>);
    } else if (props.expression instanceof DefinedExpression) {
      return this.renderFormattableExpressionWithDisambiguator(props.expression.disambiguator, props as RenderExpressionOptions<Variable>);
    } else if (props.expression instanceof FunctionParameter) {
      return formatHtmlWithoutWrapping(props.expression.textForHtml(props.boundVariableLists));
    } else if (props.expression instanceof TypeExpression) {
      return this.renderTypeExpression(props as RenderExpressionOptions<TypeExpression>);
    } else if (props.expression instanceof TypeQualifierExpression) {
      return this.renderTypeQualifierExpression(props as RenderExpressionOptions<TypeQualifierExpression>);
    } else if (props.expression instanceof PropertyExpression) {
      return this.renderPropertyExpression(props as RenderExpressionOptions<PropertyExpression>);
    } else if (props.expression instanceof RelatedObjectExpression) {
      return this.renderRelatedObjectExpression(props as RenderExpressionOptions<RelatedObjectExpression>);
    } else if (props.expression instanceof TypeRelationExpression) {
      return this.renderTypeRelationExpression(props as RenderExpressionOptions<TypeRelationExpression>);
    } else if (props.expression instanceof StandalonePropertyExpression) {
      return this.renderStandalonePropertyExpression(props as RenderExpressionOptions<StandalonePropertyExpression>);
    } else {
      throw "Could not render expression " + JSON.stringify(props.expression);
    }
  },
  renderVariable(props: RenderExpressionOptions<Variable>): SimpleReactNode[] {
    const {expression, displaySettings} = props;
    if (displaySettings && displaySettings.variableDefinitions) {
      const name = expression.getDefinition(displaySettings.variableDefinitions).name;
      const result = formatHtmlWithoutWrapping(name);
      if (expression.components.length > 0) {
        result.push("(");
        result.push(this.renderChildExpression(expression.components[0], [0], props));
        for (let i = 1; i < expression.components.length; ++i) {
          result.push(", ");
          result.push(this.renderChildExpression(expression.components[i], [i], props));
        }
        result.push(")");
      }
      return result;
    } else {
      return formatHtmlWithoutWrapping(expression.name);
    }
  },
  renderTypeExpression(props: RenderExpressionOptions<TypeExpression>): SimpleReactNode[] {
    const {expression} = props;
    const hasExplicitQualifier = !!expression.explicitQualifier;
    function getPropertyPath(propertyIndex: number) {
      const depth = numberOfObjectsAndProperties - 1 - propertyIndex;
      return [..._.times(depth, _.constant(0)), 1];
    }
    function getObjectPath(objectIndex: number) {
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
    const formattedTerm = this.renderChildExpression(
      expression.term,
      termPath,
      props,
      {
        additionalActionHighlights: filterPathsMultiple(props.actionHighlights, sharedTermHighlightPaths),
        additionalStaticHighlights: filterPathsMultiple(props.staticHighlights, sharedTermHighlightPaths)
      });
    const formattedIs = this.highlightAtMultiplePaths("is", sharedTermHighlightPaths, props);

    const articleWord = expression.properties.length ? expression.properties[0].name : expression.definition.name;
    const article = _.includes("aeiou", articleWord[0]) ? "an" : "a";
    const formattedArticle = this.highlightAtMultiplePaths(article, [typePath], props);
    const formattedName = this.highlightAtMultiplePaths(expression.definition.name, [typePath], props);

    const formattedProperties = _.flatMap(expression.properties, (p, i) => {
      const formattedProperty = this.highlightAtPath(p.name, getPropertyPath(i), props);
      if (i === 0)
        return [formattedProperty];
      else
        return [", ", formattedProperty];
    });

    const result = [formattedTerm, " ", formattedIs, " ", formattedArticle, " ", ...formattedProperties, " ", formattedName];
    if (expression.explicitQualifier) {
      const propertyIndexesRequiringQualifier = expression.properties.map((p, i) => (p.requiredParentQualifier === expression.explicitQualifier!.symbol) ? i : null).filter(isDefined);
      const objectIndexesRequiringQualifier = expression.objects.map(([d], i) => (d.requiredParentQualifier === expression.explicitQualifier!.symbol) ? i : null).filter(isDefined);
      const qualifierHighlightPaths = [qualifierPath, ...propertyIndexesRequiringQualifier.map(getPropertyPath), ...objectIndexesRequiringQualifier.map(getObjectPath)];
      const formattedQualifier = this.renderQualifier(expression.explicitQualifier.qualifier, expression.qualifierComponents, props, qualifierPath, qualifierHighlightPaths);
      result.push(" ");
      result.push(formattedQualifier);
    } else if (expression.definition.defaultQualifier) {
      const qualifierHighlightPaths = [typePath, ...propertyPaths, ...objectPaths];
      const formattedQualifier = this.renderQualifier(expression.definition.defaultQualifier, expression.qualifierComponents, props, typePath, qualifierHighlightPaths);
      result.push(" ");
      result.push(formattedQualifier);
    }
    if (expression.objects.length) {
      const formattedObjects = _.map(expression.objects, ([objectDefinition, objectTerm], index) => {
        const termPath = [...getObjectPath(index), 0];
        const renderedTerm = this.renderChildExpression(objectTerm, termPath, props);
        return this.highlightAtPath(<>{objectDefinition.name} {renderedTerm}</>, getObjectPath(index), props);
      });
      result.push(" ");
      result.push(this.highlightAtMultiplePaths("with", _.range(expression.objects.length).map(getObjectPath), props));
      result.push(" ");
      result.push(...joinAsList(formattedObjects));
    }
    return addBrackets(result, props);
  },
  renderTypeQualifierExpression(props: RenderExpressionOptions<TypeQualifierExpression>): SimpleReactNode[] {
    const {expression, path} = props;
    const formattedTerm = this.renderChildExpression(expression.term, [...path, 0], props);
    const formattedQualifier = this.renderQualifier(expression.definition.qualifier, expression.qualifierComponents, props);
    return addBrackets([formattedTerm, " is ", formattedQualifier], props);
  },
  renderPropertyExpression(props: RenderExpressionOptions<PropertyExpression>): SimpleReactNode[] {
    const {expression, path} = props;
    const formattedTerm = this.renderChildExpression(expression.term, [...path, 0], props);
    const result = [formattedTerm, " is ", expression.definition.name];
    const qualifier = _.find(expression.typeDefinition.qualifiers, q => q.symbol === expression.definition.requiredParentQualifier)?.qualifier || expression.typeDefinition.defaultQualifier;
    if (qualifier) {
      const formattedQualifier = this.renderQualifier(qualifier, expression.qualifierComponents, props);
      result.push(" ");
      result.push(formattedQualifier);
    }
    return addBrackets(result, props);
  },
  renderRelatedObjectExpression(props: RenderExpressionOptions<RelatedObjectExpression>): SimpleReactNode[] {
    const {expression, path} = props;
    const formattedTerm = this.renderChildExpression(expression.term, [...path, 0], props);
    const formattedParentTerm = this.renderChildExpression(expression.parentTerm, [...path, 1], props);
    const result = [formattedTerm, ` is ${expression.definition.article} ${expression.definition.name} for `, formattedParentTerm];
    const qualifier = _.find(expression.typeDefinition.qualifiers, q => q.symbol === expression.definition.requiredParentQualifier)?.qualifier || expression.typeDefinition.defaultQualifier;
    if (qualifier) {
      const formattedQualifier = this.renderQualifier(qualifier, expression.qualifierComponents, props);
      result.push(" ");
      result.push(formattedQualifier);
    }
    return addBrackets(result, props);
  },
  renderTypeRelationExpression(props: RenderExpressionOptions<TypeRelationExpression>): SimpleReactNode[] {
    const {expression} = props;
    const formattedFirstTerm = this.renderChildExpression(expression.firstTerm, [0], props);
    const formattedSecondTerm = this.renderChildExpression(expression.secondTerm, [1], props);
    return addBrackets([formattedFirstTerm, " " + expression.definition.linkingPhrase + " ", formattedSecondTerm], props);
  },
  renderStandalonePropertyExpression(props: RenderExpressionOptions<StandalonePropertyExpression>): SimpleReactNode[] {
    const {expression, path} = props;
    const formattedTerm = this.renderChildExpression(expression.term, [...path, 0], props);
    return addBrackets([formattedTerm, " is ", expression.definition.name], props);
  },


  renderQualifier(qualifier: QualifierDefinition, components: Expression[], props: RenderExpressionOptions<Expression>, subpath: number[] = [], sharedPaths: number[][] = []): SimpleReactNode {
    const formattedComponents = _.map(components, (component, i) => this.renderChildExpression(component, [...subpath, i + 1], props));
    return this.highlightAtMultiplePaths(
      formatHtml(qualifier.format, s => replacePlaceholders(s, formattedComponents)),
      sharedPaths,
      props);
  },

  highlight(elementToHighlight: CompoundReactNode, actionHighlights: ExpressionActionHighlight[], staticHighlights: ExpressionStaticHighlight[]): React.ReactElement {
    const matchingActionHighlight = _.find(actionHighlights, p => p.path.length === 0);
    const shouldStaticHighlight = _.some(staticHighlights, p => p.path.length === 0);
    const props: ExpressionHighlightingProps = {
      isConclusion: shouldStaticHighlight,
      isPremise: !shouldStaticHighlight && !!matchingActionHighlight,
      isClickable: !!(matchingActionHighlight && matchingActionHighlight.action)
    };
    if (!shouldStaticHighlight && matchingActionHighlight && matchingActionHighlight.action) {
      props.onClick = (e) => {
        matchingActionHighlight.action!();
        e.stopPropagation();
      }
    }
    return _.isArray(elementToHighlight) ? React.createElement(ExpressionHighlighting, props, ...elementToHighlight) : React.createElement(ExpressionHighlighting, props, elementToHighlight);
  },
  highlightAtPath(elementToHighlight: SimpleReactNode, subpath: number[], props: RenderExpressionOptions<Expression>) {
    return this.highlight(
      elementToHighlight,
      filterPaths(props.actionHighlights, subpath),
      filterPaths(props.staticHighlights, subpath));
  },
  highlightAtMultiplePaths(elementToHighlight: SimpleReactNode, subpaths: number[][], props: RenderExpressionOptions<Expression>) {
    return this.highlight(
      elementToHighlight,
      filterPathsMultiple(props.actionHighlights, subpaths),
      filterPathsMultiple(props.staticHighlights, subpaths));
  }
}
