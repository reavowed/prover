import * as _ from "lodash";

declare global {
    interface Window { definitions: { [key: string]: ExpressionDefinition } }
}

export interface ExpressionDefinition {
  symbol: string;
  baseFormatString: string;
  requiresBrackets: boolean;
  numberOfBoundVariables: number;
  structureType: string | null;
}

export type Expression = TextBasedExpression | FormatBasedExpression
export const Expression = {
  parseFromJson(json: any): Expression {
    if (typeof json === "string") {
      return new VariableOrConstant(json);
    } else if (_.isArray(json) && _.isString(json[0])) {
      const [definitionSymbol, ...boundVariablesAndComponents] = json;
      const definition = window.definitions[definitionSymbol];
      const boundVariableNames = boundVariablesAndComponents.slice(0, definition.numberOfBoundVariables);
      const components = boundVariablesAndComponents.slice(definition.numberOfBoundVariables);
      return new DefinedExpression(definition, boundVariableNames, components.map(Expression.parseFromJson));
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

abstract class TextBasedExpression {
  abstract serialize(): string
  abstract textForHtml(boundVariableLists: string[][]): string
}
abstract class FormatBasedExpression {
  abstract components: Expression[];
  abstract serialize(): string
  abstract formatForHtml(safe: boolean): string
}

export class VariableOrConstant extends TextBasedExpression {
  constructor(public name: string) {
    super()
  }
  serialize(): string {
    return this.name;
  }
  textForHtml(): string {
    return this.name;
  }
}

export class DefinedExpression extends FormatBasedExpression {
  constructor(public definition: ExpressionDefinition, public boundVariableNames: string[], public components: Expression[]) {
    super()
  }
  serialize() {
    return [this.definition.symbol, ...this.boundVariableNames, ...this.components.map(c => c.serialize())].join(" ")
  }
  formatForHtml(safe: boolean) {
    return (safe && this.definition.requiresBrackets) ?
      "(" + this.definition.baseFormatString + ")" :
      this.definition.baseFormatString;
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
    return `with (${_.map(this.components, a => a.serialize())}) ${this.name}`
  }
  formatForHtml() {
    return this.name + "(" + this.components.map((_, i) => "%" + i).join(", ") + ")";
  }
}