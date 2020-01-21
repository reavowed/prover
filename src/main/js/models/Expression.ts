import * as _ from "lodash";

declare global {
    interface Window {
      definitions: { [key: string]: ExpressionDefinition }
      typeDefinitions: { [key: string]: TypeDefinition }
    }
}

export interface ExpressionDefinition {
  symbol: string;
  baseFormatString: string;
  requiresBrackets: boolean;
  requiresComponentBrackets: boolean;
  numberOfBoundVariables: number;
  attributes: string[];
}

export interface TypeDefinition {
  symbol: string;
  name: string;
  componentFormatString: string;
  properties: { [key: string]: string }
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
  replaceAtPath(path: number[], expression: Expression): Expression
}
interface FormatBasedExpression {
  components: Expression[]
  serialize(): string
  serializeNicely(boundVariableLists: string[][]): string
  formatForHtml(parentRequiresBrackets: boolean): string
  replaceAtPath(path: number[], expression: Expression): Expression
}

export class VariableOrConstant {
  constructor(public name: string) {}
  serialize(): string {
    return this.name;
  }
  serializeNicely(_: string[][]): string {
    return this.serialize();
  }
  textForHtml(): string {
    return this.name;
  }
  replaceAtPath(path: number[], expression: Expression): Expression {
    if (!path.length) {
      return expression
    } else {
      throw "Cannot replace subexpression of variable or constant"
    }
  }
}

export class DefinedExpression {
  constructor(public definition: ExpressionDefinition, public boundVariableNames: string[], public components: Expression[]) {}
  serialize() {
    return [this.definition.symbol, ...this.boundVariableNames, ...this.components.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    const innerBoundVariables = this.boundVariableNames.length ? [this.boundVariableNames, ...boundVariableLists] : boundVariableLists;
    return [this.definition.symbol, ...this.boundVariableNames, ...this.components.map(c => c.serializeNicely(innerBoundVariables))].join(" ");
  }
  formatForHtml(parentRequiresBrackets: boolean) {
    return (parentRequiresBrackets && this.definition.requiresBrackets) ?
      "(" + this.definition.baseFormatString + ")" :
      this.definition.baseFormatString;
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
  constructor(public definition: TypeDefinition, public term: Expression, public otherComponents: Expression[], public properties: string[]) {}
  serialize(): string {
    const baseWords = ["is", this.term.serialize(), this.definition.symbol, ...this.otherComponents.map(c => c.serialize())];
    const allWords = this.properties.length ? [...baseWords, "with", "(" + this.properties.join(" ") + ")"] : baseWords;
    return allWords.join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    const baseWords = ["is", this.term.serializeNicely(boundVariableLists), this.definition.symbol, ...this.otherComponents.map(c => c.serializeNicely(boundVariableLists))];
    const allWords = this.properties.length ? [...baseWords, "with", "(" + this.properties.join(" ") + ")"] : baseWords;
    return allWords.join(" ")
  }
  addProperty(newProperty: string) {
    this.properties = [...this.properties, newProperty];
  }
  replaceAtPath(_path: number[], _expression: Expression): Expression {
    throw "Cannot replace in type expression"
  }
}

export class PropertyExpression {
  constructor(public typeDefinition: TypeDefinition, public symbol: string, public name: string, public term: Expression, public otherComponents: Expression[]) {}
  serialize(): string {
    return [this.symbol, this.term.serialize(), ...this.otherComponents.map(c => c.serialize())].join(" ")
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return [this.symbol, this.term.serialize(), ...this.otherComponents.map(c => c.serializeNicely(boundVariableLists))].join(" ")
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
    const name = boundVariableLists[this.level][this.index];
    if (_.countBy(_.flattenDeep(boundVariableLists.slice(0, this.level)))[name] > 0) { // If there are two bound variables with the same name and we're pointing to one that's further up the scope, disambiguate it
      return this.serialize()
    } else {
      return name;
    }
  }
  replaceAtPath(path: number[], expression: Expression): Expression {
    if (!path.length) {
      return expression
    } else {
      throw "Cannot replace subexpression of function parameter"
    }
  }
}

export class ExpressionApplication {
  constructor(public name: string, public components: Expression[]) {}
  serialize() {
    return `with (${this.components.map(a => a.serialize()).join(" ")}) ${this.name}`
  }
  serializeNicely(boundVariableLists: string[][]): string {
    return `with (${this.components.map(a => a.serializeNicely(boundVariableLists)).join(" ")}) ${this.name}`;
  }
  formatForHtml() {
    return this.name + "(" + this.components.map((_, i) => "%" + i).join(", ") + ")";
  }
  replaceAtPath(path: number[], expression: Expression): Expression {
    if (!path.length) {
      return expression
    } else {
      const [first, ...remaining] = path;
      const newComponents = [...this.components.slice(0, first), this.components[first].replaceAtPath(remaining, expression), ... this.components.slice(first + 1)];
      return new ExpressionApplication(this.name, newComponents);
    }
  }
}
