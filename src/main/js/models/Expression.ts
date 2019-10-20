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
export const Expression = {
  parseFromJson(json: any): Expression {
    if (typeof json === "string") {
      const definition = window.definitions[json];
      if (definition) {
        return new DefinedExpression(definition, [], [])
      } else {
      return new VariableOrConstant(json);
      }
    } else if (_.isArray(json) && _.isString(json[0])) {
      const [definitionSymbol, ...boundVariablesAndComponents] = json;
      const expressionDefinition = window.definitions[definitionSymbol];
      const typeDefinition = window.typeDefinitions[definitionSymbol];
      const parentTypeDefinition = _.find(window.typeDefinitions, d => _.has(d.properties, definitionSymbol));
      if (expressionDefinition) {
        const boundVariableNames = boundVariablesAndComponents.slice(0, expressionDefinition.numberOfBoundVariables);
        const componentsJson = boundVariablesAndComponents.slice(expressionDefinition.numberOfBoundVariables);
        if (_.includes(expressionDefinition.attributes, "conjunction")) {
          const [firstComponentJson, secondComponentJson] = componentsJson;
          const firstComponent = Expression.parseFromJson(firstComponentJson);
          if (firstComponent instanceof TypeExpression && _.isArray(secondComponentJson) && _.isString(secondComponentJson[0])) {
            const [propertyName, termJson, ...otherComponentsJson] = secondComponentJson;
            const property = firstComponent.definition.properties[propertyName];
            const term = Expression.parseFromJson(termJson);
            const otherComponents = otherComponentsJson.map(Expression.parseFromJson);
            if (property && term.serialize() === firstComponent.term.serialize() && otherComponents.map(c => c.serialize()).join(" ") === firstComponent.otherComponents.map(c => c.serialize()).join(" ")) {
              firstComponent.addProperty(property);
              return firstComponent;
            }
          }
        }
        return new DefinedExpression(expressionDefinition, boundVariableNames, componentsJson.map(Expression.parseFromJson));
      } else if (typeDefinition) {
        return new TypeExpression(typeDefinition, Expression.parseFromJson(boundVariablesAndComponents[0]), boundVariablesAndComponents.slice(1).map(Expression.parseFromJson), [])
      } else if (parentTypeDefinition) {
        return new PropertyExpression(parentTypeDefinition, definitionSymbol, parentTypeDefinition.properties[definitionSymbol], Expression.parseFromJson(boundVariablesAndComponents[0]), boundVariablesAndComponents.slice(1).map(Expression.parseFromJson))
      } else {
        throw `Unrecognised definition ${definitionSymbol}`
      }
    } else if (_.isArray(json) && _.isNumber(json[0])) {
      const [level, index] = json;
      return new FunctionParameter(level, index);
    } else if (_.isObject(json)) {
      let [[name, args]] = _.toPairs(json);
      return new ExpressionApplication(name, args.map(Expression.parseFromJson));
    } else {
      throw `Unrecognised expression ${JSON.stringify(json)}`
    }
  }
};

interface TextBasedExpression {
  serialize(): string
  textForHtml(boundVariableLists: string[][]): string
}
interface FormatBasedExpression {
  components: Expression[]
  serialize(): string
  formatForHtml(parentRequiresBrackets: boolean): string
}

export class VariableOrConstant {
  constructor(public name: string) {}
  serialize(): string {
    return this.name;
  }
  textForHtml(): string {
    return this.name;
  }
}

export class DefinedExpression {
  constructor(public definition: ExpressionDefinition, public boundVariableNames: string[], public components: Expression[]) {}
  serialize() {
    return [this.definition.symbol, ...this.boundVariableNames, ...this.components.map(c => c.serialize())].join(" ")
  }
  formatForHtml(parentRequiresBrackets: boolean) {
    return (parentRequiresBrackets && this.definition.requiresBrackets) ?
      "(" + this.definition.baseFormatString + ")" :
      this.definition.baseFormatString;
  }
}

export class TypeExpression {
  constructor(public definition: TypeDefinition, public term: Expression, public otherComponents: Expression[], public properties: string[]) {}
  serialize(): string {
    const baseWords = ["is", this.term.serialize(), this.definition.symbol, ...this.otherComponents.map(c => c.serialize())];
    const allWords = this.properties.length ? [...baseWords, "with", "(" + this.properties.join(" ") + ")"] : baseWords;
    return allWords.join(" ")
  }
  addProperty(newProperty: string) {
    this.properties = [...this.properties, newProperty];
  }
}

export class PropertyExpression {
  constructor(public typeDefinition: TypeDefinition, public symbol: string, public name: string, public term: Expression, public otherComponents: Expression[]) {}
  serialize(): string {
    return [this.symbol, this.term.serialize(), ...this.otherComponents.map(c => c.serialize())].join(" ")
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
  textForHtml(boundVariableLists: string[][]) {
    const name = boundVariableLists[this.level][this.index];
    if (_.countBy(_.flattenDeep(boundVariableLists))[name] > 1) { // Disambiguate variables that appear multiple times in scope
      return this.serialize()
    } else {
      return name;
    }
  }
}

export class ExpressionApplication {
  constructor(public name: string, public components: Expression[]) {}
  serialize() {
    return `with (${this.components.map(a => a.serialize()).join(" ")}) ${this.name}`
  }
  formatForHtml() {
    return this.name + "(" + this.components.map((_, i) => "%" + i).join(", ") + ")";
  }
}
