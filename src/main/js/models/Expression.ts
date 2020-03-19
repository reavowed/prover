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
  properties: { [key: string]: PropertyDefinition }
}

export interface ExpressionMatchResult {
  type: "expression";
  matchedVariable: string;
  expression: Expression;
  pathWithinMatch: number[];
  boundVariablesWithinMatch: string[][];
}

export interface BoundVariableMatchResult {
  type: "boundVariable";
  name: string;
  index: number;
  pathWithinMatch: number[];
}

type MatchResult = ExpressionMatchResult | BoundVariableMatchResult;

export function matchTemplate(template: any, expression: Expression, pathWithinMatch: number[], boundVariablesWithinMatch: string[][]): MatchResult[] | undefined {
  if (_.isString(template)) {
    return [{type: "expression", matchedVariable: template, expression, pathWithinMatch, boundVariablesWithinMatch}];
  } else if (_.isObject(template) && !_.isArray(template)) {
    const [[name, args]] = _.toPairs(template);
    if (args.length == 0) {
      return [{type: "expression", matchedVariable: name, expression, pathWithinMatch, boundVariablesWithinMatch}];
    }
  } else if (_.isArray(template) && _.isString(template[0])) {
    if ((expression instanceof DefinedExpression) && (expression.definition.symbol === template[0])) {
      const innerBoundVariables = expression.definition.numberOfBoundVariables ? [expression.boundVariableNames, ...boundVariablesWithinMatch] : boundVariablesWithinMatch;
      const componentMatches = _.chain(template.slice(1))
          .zip(expression.components)
          .map(([t, c], i) => matchTemplate(t, c!, [...pathWithinMatch, i], innerBoundVariables))
          .value();
      if (_.every(componentMatches)) {
        const boundVariableMatches = expression.boundVariableNames.map((name, index) => ({type: "boundVariable", name, index, pathWithinMatch} as BoundVariableMatchResult));
        const flattenedComponentMatches: MatchResult[] = _.flatten(componentMatches as MatchResult[][]);
        return [...boundVariableMatches, ...flattenedComponentMatches];
      }
    } else if ((expression instanceof TypeExpression) && (expression.definition.symbol === template[0])) {
      const componentMatches = _.chain(template.slice(1))
          .zip([expression.term, ...expression.otherComponents])
          .map(([t, c], i) => matchTemplate(t, c!, [...pathWithinMatch, i], boundVariablesWithinMatch))
          .value();
      if (_.every(componentMatches)) {
        return _.flatten(componentMatches as MatchResult[][]);
      }
    }
  } else if (_.isArray(template) && _.isNumber(template[0])) {
    if ((expression instanceof FunctionParameter) && _.isEqual(template, [expression.level, expression.index])) {
      return [];
    }
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
  constructor(public definition: TypeDefinition, public term: Expression, public otherComponents: Expression[], public properties: PropertyDefinition[]) {}
  serialize(): string {
    const baseWords = ["is", this.term.serialize(), this.definition.symbol, ...this.otherComponents.map(c => c.serialize())];
    const allWords = this.properties.length ? [...baseWords, "with", "(" + this.properties.map(p => p.symbol).join(" ") + ")"] : baseWords;
    return allWords.join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    const baseWords = ["is", this.term.serializeNicely(boundVariableLists), this.definition.symbol, ...this.otherComponents.map(c => c.serializeNicely(boundVariableLists))];
    const allWords = this.properties.length ? [...baseWords, "with", "(" + this.properties.map(p => p.symbol).join(" ") + ")"] : baseWords;
    return allWords.join(" ")
  }
  addProperty(newProperty: PropertyDefinition) {
    this.properties = [...this.properties, newProperty];
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
        return new TypeExpression(this.definition, this.term.replaceAtPath(remaining, expression), this.otherComponents, this.properties);
      else
        return new TypeExpression(this.definition, this.term, [...this.otherComponents.slice(0, first - 1), this.otherComponents[first - 1].replaceAtPath(remaining, expression), ... this.otherComponents.slice(first)], this.properties);
    } else {
      throw "Replacing in type expression with properties not implemented"
    }
  }
}

export class PropertyExpression {
  constructor(public typeDefinition: TypeDefinition, public symbol: string, public definition: PropertyDefinition, public term: Expression, public otherComponents: Expression[]) {}
  serialize(): string {
    return [this.symbol, this.term.serialize(), ...this.otherComponents.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return [this.symbol, this.term.serialize(), ...this.otherComponents.map(c => c.serializeNicely(boundVariableLists))].join(" ")
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
