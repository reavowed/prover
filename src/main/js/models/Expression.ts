import * as _ from "lodash";
import {mapAtIndex, replaceAtIndex} from "./Helpers";

declare global {
    interface Window {
      definitions: { [key: string]: ExpressionDefinition }
      typeDefinitions: { [key: string]: TypeDefinition }
    }
}

export interface PropertyDefinition {
  symbol: String,
  qualifiedSymbol: String,
  name: String
}

export interface ExpressionDefinition {
  symbol: string;
  baseFormatString: string;
  requiresBrackets: boolean;
  requiresComponentBrackets: boolean;
  numberOfBoundVariables: number;
  numberOfComponents: number;
  attributes: string[];
}

export interface TypeDefinition {
  symbol: string;
  name: string;
  numberOfComponents: number;
  componentFormatString: string;
  properties: PropertyDefinition[]
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
    const componentMatches = _.chain([template.term, ...template.otherComponents])
        .zip([expression.term, ...expression.otherComponents])
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

export type Expression = TextBasedExpression | FormatBasedExpression | TypeExpression | PropertyExpression

interface TextBasedExpression {
  serialize(): string
  serializeNicely(boundVariableLists: string[][]): string
  textForHtml(boundVariableLists: string[][]): string
  setBoundVariableName(newName: string, index: number, path: number[]): Expression
  replaceAtPath(path: number[], expression: Expression): Expression
}
interface FormatBasedExpression {
  components: Expression[]
  serialize(): string
  serializeNicely(boundVariableLists: string[][]): string
  formatForHtml(parentRequiresBrackets: boolean): string
  setBoundVariableName(newName: string, index: number, path: number[]): Expression
  replaceAtPath(path: number[], expression: Expression): Expression
}

export class Variable {
  constructor(public name: string, public components: Expression[]) {}
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
        this.name :
        this.name + "(" + this.components.map((_, i) => "%" + i).join(", ") + ")";
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in variable"
  }
  replaceAtPath(path: number[], expression: Expression): Expression {
    if (!path.length) {
      return expression
    } else {
      const [first, ...remaining] = path;
      const newComponents = [...this.components.slice(0, first), this.components[first].replaceAtPath(remaining, expression), ... this.components.slice(first + 1)];
      return new Variable(this.name, newComponents);
    }
  }
}

export class DefinedExpression {
  constructor(public definition: ExpressionDefinition, public boundVariableNames: string[], public components: Expression[]) {}
  serialize() {
    return [this.definition.symbol, ...this.boundVariableNames, ...this.components.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    const innerBoundVariables = this.boundVariableNames.length ? [...boundVariableLists, this.boundVariableNames] : boundVariableLists;
    return [this.definition.symbol, ...this.boundVariableNames, ...this.components.map(c => c.serializeNicely(innerBoundVariables))].join(" ");
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
  replaceAtPath(path: number[], expression: Expression): Expression {
    if (!path.length) {
      return expression
    } else {
      const [first, ...remaining] = path;
      const newComponents = [...this.components.slice(0, first), this.components[first].replaceAtPath(remaining, expression), ... this.components.slice(first + 1)];
      return new DefinedExpression(this.definition, this.boundVariableNames, newComponents);
    }
  }
}

export class TypeExpression {
  constructor(public definition: TypeDefinition, public term: Expression, public otherComponents: Expression[], public properties: PropertyDefinition[], public conjunctionDefinition: ExpressionDefinition | undefined) {}
  serialize(): string {
    const termAndComponentsWords = [this.term.serialize(), ...this.otherComponents.map(c => c.serialize())];
    const baseWords = [this.definition.symbol, ...termAndComponentsWords];
    const allWords = _.reduce(
          this.properties,
        // @ts-ignore
          (wordsSoFar: string[], propertyDefinition: PropertyDefinition) => {
            if (this.conjunctionDefinition) {
              return [this.conjunctionDefinition.symbol, ...wordsSoFar, propertyDefinition.qualifiedSymbol, ...termAndComponentsWords]
            } else throw "Cannot serialize type with property without conjunction";
          },
          baseWords);
    return allWords.join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    const baseWords = ["is", this.term.serializeNicely(boundVariableLists), this.definition.symbol, ...this.otherComponents.map(c => c.serializeNicely(boundVariableLists))];
    const allWords = this.properties.length ? [...baseWords, "with", "(" + this.properties.map(p => p.symbol).join(" ") + ")"] : baseWords;
    return allWords.join(" ")
  }
  addProperty(newProperty: PropertyDefinition, conjunctionDefinition: ExpressionDefinition) {
    this.properties = [...this.properties, newProperty];
    this.conjunctionDefinition = conjunctionDefinition;
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in type expression"
  }
  replaceAtPath(path: number[], expression: Expression): Expression {
    if (!path.length) {
      return expression
    } else if (!this.properties.length) {
      const [first, ...remaining] = path;
      if (first == 0)
        return new TypeExpression(this.definition, this.term.replaceAtPath(remaining, expression), this.otherComponents, this.properties, this.conjunctionDefinition);
      else
        return new TypeExpression(this.definition, this.term, [...this.otherComponents.slice(0, first - 1), this.otherComponents[first - 1].replaceAtPath(remaining, expression), ... this.otherComponents.slice(first)], this.properties, this.conjunctionDefinition);
    } else {
      throw "Replacing in type expression with properties not implemented"
    }
  }
}

export class PropertyExpression {
  constructor(public typeDefinition: TypeDefinition, public definition: PropertyDefinition, public term: Expression, public otherComponents: Expression[]) {}
  serialize(): string {
    return [this.definition.qualifiedSymbol, this.term.serialize(), ...this.otherComponents.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return [this.definition.qualifiedSymbol, this.term.serialize(), ...this.otherComponents.map(c => c.serializeNicely(boundVariableLists))].join(" ")
  }
  setBoundVariableName(): Expression {
    throw "Cannot set bound variable name in property expression"
  }
  replaceAtPath(_path: number[], _expression: Expression): Expression {
    throw "Cannot replace in property expression"
  }
}

export class FunctionParameter {
  constructor(public level: number, public index: number) {
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
  replaceAtPath(path: number[], expression: Expression): Expression {
    if (!path.length) {
      return expression
    } else {
      throw "Cannot replace subexpression of function parameter"
    }
  }
}
