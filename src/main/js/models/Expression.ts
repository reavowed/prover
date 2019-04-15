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
  numberOfBoundVariables: number;
  attributes: string[];
}

export interface TypeDefinition {
  symbol: string;
  name: string;
  componentFormatString: string;
  properties: { [key: string]: string }
}

export type Expression = TextBasedExpression | FormatBasedExpression | TypeExpression
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
  formatForHtml(safe: boolean): string
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
  formatForHtml(safe: boolean) {
    return (safe && this.definition.requiresBrackets) ?
      "(" + this.definition.baseFormatString + ")" :
      this.definition.baseFormatString;
  }
}

export class TypeExpression {
  constructor(public definition: TypeDefinition, public term: Expression, public otherComponents: Expression[], public properties: string[]) {}
  serialize(): string {
    return [this.definition.symbol, this.term.serialize(), ...this.otherComponents.map(c => c.serialize())].join(" ")
  }
  addProperty(newProperty: string) {
    this.properties = [...this.properties, newProperty];
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
