import * as _ from "lodash";
import {mapAtIndex, replaceAtIndex} from "./Helpers";

declare global {
    interface Window {
      definitions: { [key: string]: ExpressionDefinition }
      typeDefinitions: { [key: string]: TypeDefinition }
    }
}

export interface DisambiguatedSymbol {
  baseSymbol: string;
  disambiguator: string | null;
  serialized: string;
}

export interface ExpressionDefinition {
  symbol: DisambiguatedSymbol;
  baseFormatString: string;
  requiresBrackets: boolean;
  requiresComponentBrackets: boolean;
  numberOfBoundVariables: number;
  numberOfComponents: number;
  attributes: string[];
}

export interface QualifierDefinition {
  numberOfComponents: number;
  format: string;
}

export interface TypeDefinition {
  symbol: string;
  name: string;
  defaultQualifier: QualifierDefinition | null;
  properties: PropertyDefinition[];
  qualifiers: TypeQualifierDefinition[];
}

export interface TypeQualifierDefinition {
  symbol: string;
  qualifiedSymbol: string;
  name: string;
  qualifier: QualifierDefinition;
}

export interface PropertyDefinition {
  symbol: string;
  qualifiedSymbol: string;
  name: string;
  requiredParentQualifier: string;
}

export interface StandalonePropertyDefinition {
  symbol: string;
  qualifiedSymbol: string;
  name: string;
  numberOfComponents: number;
  componentFormatString: string;
}

export class ExpressionMatchResult {
  constructor(public matchedVariable: string, public expression: Expression, public pathWithinMatch: number[], public boundVariablesWithinMatch: string[][]) {}
  type = "expression";
}

export interface BoundVariableMatchResult {
  type: "boundVariable";
  name: string;
  index: number;
  pathWithinMatch: number[];
}

type MatchResult = ExpressionMatchResult | BoundVariableMatchResult;

export function tokenize(str: string): string[]  {
  function splitToken(word: string): string[] {
    const firstSingleCharacterIndex = _.findIndex(word, x => _.includes("(){}", x));
    if (firstSingleCharacterIndex === 0) {
      return [word[0], ...splitToken(word.substring(1))];
    } else if (firstSingleCharacterIndex > 0) {
      return [word.substring(0, firstSingleCharacterIndex), word[firstSingleCharacterIndex], ...splitToken(word.substring(firstSingleCharacterIndex + 1))];
    } else if (word.length) {
      return [word];
    } else {
      return [];
    }
  }
  return _.flatMap(str.split(' '), splitToken);
}

function checkComponentsMatch(matchResults: MatchResult[]): MatchResult[] | undefined {
  const expressionMatchResults = <ExpressionMatchResult[]>matchResults.filter(a => a instanceof ExpressionMatchResult);
  const matchesByVariable = <{[index: string]: ExpressionMatchResult[]}>_.groupBy(expressionMatchResults, r => r.matchedVariable);
  if (_.every(matchesByVariable, rs => _.uniq(_.map(rs, r => r.expression.serialize())).length == 1)) {
    return matchResults;
  } else {
    return undefined;
  }
}

export function matchTemplate(template: Expression, expression: Expression, pathWithinMatch: number[], boundVariablesWithinMatch: string[][]): MatchResult[] | undefined {
  if (template instanceof Variable && template.components.length === 0) {
    return [new ExpressionMatchResult(template.name, expression, pathWithinMatch, boundVariablesWithinMatch)];
  } else if (template instanceof DefinedExpression && expression instanceof DefinedExpression && expression.definition === template.definition) {
      const innerBoundVariables = expression.definition.numberOfBoundVariables ? [expression.boundVariableNames, ...boundVariablesWithinMatch] : boundVariablesWithinMatch;
      const componentMatches = _.chain(template.components)
          .zip(expression.components)
          .map(([t, c], i) => t && matchTemplate(t, c!, [...pathWithinMatch, i], innerBoundVariables))
          .value();
      if (_.every(componentMatches)) {
        const boundVariableMatches = expression.boundVariableNames.map((name, index) => ({type: "boundVariable", name, index, pathWithinMatch} as BoundVariableMatchResult));
        const flattenedComponentMatches: MatchResult[] = _.flatten(componentMatches as MatchResult[][]);
        return checkComponentsMatch([...boundVariableMatches, ...flattenedComponentMatches]);
      }
  } else if (template instanceof TypeExpression && expression instanceof TypeExpression && expression.definition === template.definition) {
    const componentMatches = _.chain([template.term, ...template.qualifierComponents])
        .zip([expression.term, ...expression.qualifierComponents])
        .map(([t, c], i) => t && matchTemplate(t, c!, [...pathWithinMatch, i], boundVariablesWithinMatch))
        .value();
    if (_.every(componentMatches)) {
      return checkComponentsMatch(_.flatten(componentMatches as MatchResult[][]));
    }
  } else if (template instanceof FunctionParameter && expression instanceof FunctionParameter && template.index === expression.index && template.level === expression.level) {
    return [];
  }
  return undefined;
}

export abstract class Expression {
  abstract serialize(): string
  abstract serializeNicely(boundVariableLists: string[][]): string
  abstract setBoundVariableName(newName: string, index: number, path: number[]): Expression
  abstract replaceAtPath(path: number[], expression: Expression): [Expression, number[][]]
  abstract getDisambiguators(): string[]
}

export class Variable extends Expression {
  constructor(public name: string, public components: Expression[]) { super(); }
  symbol: string = this.name;
  disambiguator: string | null = null;
  serialize() {
    return this.components.length == 0 ?
        this.name :
        `with (${this.components.map(a => a.serialize()).join(" ")}) ${this.name}`;
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return this.components.length == 0 ?
        this.name :
        `with (${this.components.map(a => a.serializeNicely(boundVariableLists)).join(" ")}) ${this.name}`;
  }
  formatForHtml() {
    return this.components.length == 0 ?
        "%0" :
        "%0" + "(" + this.components.map((_, i) => "%" + (i+1)).join(", ") + ")";
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in variable"
  }
  replaceAtPath(path: number[], expression: Expression): [Expression, number[][]] {
    if (!path.length) {
      return [expression, [path]];
    } else {
      const [first, ...remaining] = path;
      const [replacedComponent, replacementPaths] = this.components[first].replaceAtPath(remaining, expression);
      const newComponents = [...this.components.slice(0, first), replacedComponent, ... this.components.slice(first + 1)];
      return [new Variable(this.name, newComponents), replacementPaths.map(p => [first, ...p])];
    }
  }
  getDisambiguators(): string[] {
    const componentDisambiguators = _.uniq(_.flatMap(this.components, x => x.getDisambiguators()));
    return this.disambiguator ?
        _.uniq([this.disambiguator, ...componentDisambiguators]) :
        componentDisambiguators;
  }
}

export class DefinedExpression extends Expression {
  constructor(public definition: ExpressionDefinition, public boundVariableNames: string[], public components: Expression[]) { super(); }
  symbol: string = this.definition.symbol.baseSymbol;
  disambiguator: string | null = this.definition.symbol.disambiguator;
  serialize() {
    return [this.definition.symbol.serialized, ...this.boundVariableNames, ...this.components.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    const innerBoundVariables = this.boundVariableNames.length ? [...boundVariableLists, this.boundVariableNames] : boundVariableLists;
    return [this.definition.symbol.serialized, ...this.boundVariableNames, ...this.components.map(c => c.serializeNicely(innerBoundVariables))].join(" ");
  }
  formatForHtml(parentRequiresBrackets: boolean) {
    return (parentRequiresBrackets && this.definition.requiresBrackets) ?
      "(" + this.definition.baseFormatString + ")" :
      this.definition.baseFormatString;
  }
  setBoundVariableName(newName: string, index: number, path: number[]): Expression {
    if (path.length == 0) {
      return new DefinedExpression(this.definition, replaceAtIndex(this.boundVariableNames, index, newName), this.components);
    } else {
      return new DefinedExpression(this.definition, this.boundVariableNames, mapAtIndex(this.components, path[0], c => c.setBoundVariableName(newName, index, path.slice(1))));
    }
  }
  replaceAtPath(path: number[], expression: Expression): [Expression, number[][]] {
    if (!path.length) {
      return [expression, [path]];
    } else {
      const [first, ...remaining] = path;
      const [replacedComponent, replacedPaths] = this.components[first].replaceAtPath(remaining, expression);
      const newComponents = [...this.components.slice(0, first), replacedComponent, ... this.components.slice(first + 1)];
      return [new DefinedExpression(this.definition, this.boundVariableNames, newComponents), replacedPaths.map(p => [first, ...p])];
    }
  }
  getDisambiguators(): string[] {
    const componentDisambiguators = _.uniq(_.flatMap(this.components, x => x.getDisambiguators()));
    return this.disambiguator ?
      _.uniq([this.disambiguator, ...componentDisambiguators]) :
      componentDisambiguators;
  }
}

export class TypeExpression extends Expression {
  constructor(public definition: TypeDefinition, public term: Expression, public explicitQualifier: TypeQualifierDefinition | undefined, public qualifierComponents: Expression[],  public properties: PropertyDefinition[], public conjunctionDefinition: ExpressionDefinition | undefined) { super(); }
  serialize(): string {
    const serializedTerm = this.term.serialize();
    const serializedQualifierComponents = this.qualifierComponents.map(c => c.serialize());

    let serializedBaseComponents: string[], wordsWithQualifier;

    if (this.explicitQualifier) {
      serializedBaseComponents = [serializedTerm];
      if (this.conjunctionDefinition) {
        wordsWithQualifier = [this.conjunctionDefinition.symbol.serialized, this.definition.symbol, serializedTerm, this.explicitQualifier.qualifiedSymbol, serializedTerm, ...serializedQualifierComponents]
      } else throw "Cannot serialize type with property without conjunction";
    } else {
      serializedBaseComponents = [serializedTerm, ...serializedQualifierComponents];
      wordsWithQualifier = [this.definition.symbol, ...serializedBaseComponents];
    }

    const allWords = _.reduce(
        this.properties,
        // @ts-ignore
        (wordsSoFar: string[], propertyDefinition: PropertyDefinition) => {
          if (this.conjunctionDefinition) {
            return [this.conjunctionDefinition.symbol.serialized, ...wordsSoFar, propertyDefinition.qualifiedSymbol, ...serializedBaseComponents]
          } else throw "Cannot serialize type with property without conjunction";
        },
        wordsWithQualifier);
    return allWords.join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    const baseWords = ["is", this.term.serializeNicely(boundVariableLists), this.definition.symbol]
    const worldsWithQualifier = this.explicitQualifier ?
        [...baseWords, this.explicitQualifier.symbol, ...this.qualifierComponents.map(c => c.serializeNicely(boundVariableLists))] :
        [...baseWords, ...this.qualifierComponents.map(c => c.serializeNicely(boundVariableLists))];
    const allWords = this.properties.length ? [...worldsWithQualifier, "with", "(" + this.properties.map(p => p.symbol).join(" ") + ")"] : worldsWithQualifier;
    return allWords.join(" ")
  }
  addQualifier(newQualifier: TypeQualifierDefinition, qualifierComponents: Expression[], conjunctionDefinition: ExpressionDefinition) {
    this.explicitQualifier = newQualifier;
    this.qualifierComponents = qualifierComponents;
    this.conjunctionDefinition = conjunctionDefinition;
  }
  addProperty(newProperty: PropertyDefinition, conjunctionDefinition: ExpressionDefinition) {
    this.properties = [...this.properties, newProperty];
    this.conjunctionDefinition = conjunctionDefinition;
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in type expression"
  }
  replaceAtPath(path: number[], expression: Expression): [Expression, number[][]] {
    if (!path.length) {
      return [expression, [path]];
    }

    const hasExplicitQualifier = !!this.explicitQualifier;

    const getMainExpressionPath = () => Array(this.properties.length + (hasExplicitQualifier ? 1 : 0)).fill(0);
    const getExplicitQualifierExpressionPath = () => [...Array(this.properties.length).fill(0), 1];
    const getPropertyExpressionPath = (index: number) => [...Array(index).fill(0), 1];

    const replaceInMainExpression = (innerPath: number[]) => {
      const [first, ...remaining] = innerPath;
      if (first == 0) {
        return replaceInTerm(remaining);
      } else if (!hasExplicitQualifier) {
        return replaceInQualifierComponent(first - 1, remaining);
      } else {
        throw "Invalid replacement of explicit qualifier component"
      }
    };
    const replaceInTerm = (innerPath: number[]) => {
      const [replacedTerm, replacedPathsInTerm] = this.term.replaceAtPath(innerPath, expression);
      const replacedExpression = new TypeExpression(this.definition, replacedTerm, this.explicitQualifier, this.qualifierComponents, this.properties, this.conjunctionDefinition);
      const getReplacementPaths = (outerPath: number[]) => replacedPathsInTerm.map(p => [...outerPath, 0, ...p]);
      const replacedPathsInMainExpression = getReplacementPaths(getMainExpressionPath());
      const replacedPathsInQualifierExpression = hasExplicitQualifier ?
          getReplacementPaths(getExplicitQualifierExpressionPath()) :
          [];
      const replacedPathsInPropertyExpressions = _.flatMap(_.range(this.properties.length), i => getReplacementPaths(getPropertyExpressionPath(i)));
      const result: [Expression, number[][]] = [replacedExpression, [...replacedPathsInMainExpression, ...replacedPathsInQualifierExpression, ...replacedPathsInPropertyExpressions]];
      return result;
    };
    const replaceInQualifierComponent = (componentIndex: number, innerPath: number[]) => {
      const [replacedComponent, replacedPathsInComponent] = this.qualifierComponents[componentIndex].replaceAtPath(innerPath, expression);
      const replacedComponents = [...this.qualifierComponents.slice(0, componentIndex), replacedComponent, ... this.qualifierComponents.slice(componentIndex + 1)];
      const replacedExpression = new TypeExpression(this.definition, this.term, this.explicitQualifier, replacedComponents, this.properties, this.conjunctionDefinition);
      const getReplacementPaths = (outerPath: number[]) => replacedPathsInComponent.map(p => [...outerPath, componentIndex + 1, ...p]);
      if (hasExplicitQualifier) {
        const result: [Expression, number[][]] =  [replacedExpression, getReplacementPaths(getExplicitQualifierExpressionPath())];
        return result;
      } else {
        const replacedPathsInMainExpression = getReplacementPaths(getMainExpressionPath());
        const replacedPathsInPropertyExpressions = _.flatMap(_.range(this.properties.length), i => getReplacementPaths(getPropertyExpressionPath(i)));
        const result: [Expression, number[][]] =  [replacedExpression, [...replacedPathsInMainExpression, ...replacedPathsInPropertyExpressions]];
        return result;
      }
    };

    // @ts-ignore
    if (!_.startsWith(path, Array(this.properties.length).fill(0))) {
      throw "Cannot replace directly in property expression"
    }
    const pathAfterProperties = path.slice(this.properties.length);
    if (hasExplicitQualifier) {
      const [first, ...remaining] = pathAfterProperties;
      if (first == 0) {
        return replaceInMainExpression(remaining);
      } else if (first == 1) {
        const [next, ...moreRemaining] = remaining;
        if (next > 0) {
          return replaceInQualifierComponent(next - 1, moreRemaining);
        }
      }
      throw "Invalid replacement path";
    } else {
      return replaceInMainExpression(pathAfterProperties);
    }
  }
  getDisambiguators(): string[] {
    return _.uniq(_.flatMap([this.term, ...this.qualifierComponents], x => x.getDisambiguators()));
  }
}

export class TypeQualifierExpression extends Expression {
  constructor(public definition: TypeQualifierDefinition, public typeDefinition: TypeDefinition, public term: Expression, public qualifierComponents: Expression[]) { super(); }
  serialize(): string {
    return [this.definition.qualifiedSymbol, this.term.serialize(), ...this.qualifierComponents.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return [this.definition.qualifiedSymbol, this.term.serialize(), ...this.qualifierComponents.map(c => c.serializeNicely(boundVariableLists))].join(" ")
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in qualifier expression"
  }
  replaceAtPath(_path: number[], _expression: Expression): [Expression, number[][]] {
    throw "Cannot replace in qualifier expression"
  }
  getDisambiguators(): string[] {
    return _.uniq(_.flatMap([this.term, ...this.qualifierComponents], x => x.getDisambiguators()));
  }
}

export class PropertyExpression extends Expression {
  constructor(public definition: PropertyDefinition, public typeDefinition: TypeDefinition, public term: Expression, public qualifierComponents: Expression[]) { super(); }
  serialize(): string {
    return [this.definition.qualifiedSymbol, this.term.serialize(), ...this.qualifierComponents.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return [this.definition.qualifiedSymbol, this.term.serializeNicely(boundVariableLists), ...this.qualifierComponents.map(c => c.serializeNicely(boundVariableLists))].join(" ")
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in property expression"
  }
  replaceAtPath(_path: number[], _expression: Expression): [Expression, number[][]] {
    throw "Cannot replace in property expression"
  }
  getDisambiguators(): string[] {
    return _.uniq(_.flatMap([this.term, ...this.qualifierComponents], x => x.getDisambiguators()));
  }
}

export class StandalonePropertyExpression extends Expression {
  constructor(public definition: StandalonePropertyDefinition, public term: Expression, public qualifierComponents: Expression[]) { super(); }
  serialize(): string {
    return [this.definition.qualifiedSymbol, this.term.serialize(), ...this.qualifierComponents.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return [this.definition.qualifiedSymbol, this.term.serialize(), ...this.qualifierComponents.map(c => c.serializeNicely(boundVariableLists))].join(" ")
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in property expression"
  }
  replaceAtPath(_path: number[], _expression: Expression): [Expression, number[][]] {
    throw "Cannot replace in property expression"
  }
  getDisambiguators(): string[] {
    return _.uniq(_.flatMap([this.term, ...this.qualifierComponents], x => x.getDisambiguators()));
  }
}

export class FunctionParameter extends Expression {
  constructor(public level: number, public index: number) {
    super();
    this.level = level;
    this.index = index;
  }
  serialize() {
    return "$".repeat(this.level + 1) + this.index;
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return this.textForHtml(boundVariableLists);
  }
  textForHtml(boundVariableLists: string[][]) {
    const name = boundVariableLists[boundVariableLists.length - this.level - 1][this.index];
    if (_.countBy(_.flattenDeep(boundVariableLists.slice(boundVariableLists.length - this.level)))[name] > 0) { // If there are two bound variables with the same name and we're pointing to one that's further up the scope, disambiguate it
      return this.serialize()
    } else {
      return name;
    }
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in function parameter"
  }
  replaceAtPath(path: number[], expression: Expression): [Expression, number[][]] {
    if (!path.length) {
      return [expression, [path]];
    } else {
      throw "Cannot replace subexpression of function parameter"
    }
  }
  getDisambiguators(): string[] {
    return [];
  }
}
