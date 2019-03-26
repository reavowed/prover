import _ from "lodash";

export class VariableOrConstant {
  constructor(name) {
    this.name = name;
  }
  serialize() {
    return this.name;
  }
  textForHtml() {
    return this.name;
  }
}
export class DefinedExpression {
  constructor(definition, boundVariableNames, components) {
    this.definition = definition;
    this.boundVariableNames = boundVariableNames;
    this.components = components;
  }
  serialize() {
    return [this.definition.symbol, ...this.boundVariableNames, ...this.components.map(c => c.serialize())].join(" ")
  }
  formatForHtml(safe) {
    return (safe && this.definition.requiresBrackets) ?
      "(" + this.definition.baseFormatString + ")" :
      this.definition.baseFormatString;
  }
}
export class FunctionParameter {
  constructor(level, index) {
    this.level = level;
    this.index = index;
  }
  serialize() {
    return "$".repeat(this.level + 1) + this.index;
  }
  textForHtml(boundVariableLists) {
    const name = boundVariableLists[this.level][this.index];
    if (_.countBy(_.flattenDeep(boundVariableLists))[name] > 1) { // Disambiguate variables that appear multiple times in scope
      return this.serialize()
    } else {
      return name;
    }
  }
}
export class ExpressionApplication {
  constructor(name, components) {
    this.name = name;
    this.components = components;
  }
  serialize() {
    return `with (${_.map(this.components, a => a.serialize())}) ${this.name}`
  }
  formatForHtml() {
    return this.name + "(" + this.components.map((_, i) => "%" + i).join(", ") + ")";
  }
}

export class Parser {
  static parseExpression(rawExpression) {
    if (typeof rawExpression === "string") { // Variable or constant
      return new VariableOrConstant(rawExpression);
    } else if (_.isArray(rawExpression) && _.isString(rawExpression[0])) { // Defined statement or term
      const [definitionSymbol, ...boundVariablesAndComponents] = rawExpression;
      const definition = window.definitions[definitionSymbol];
      const boundVariableNames = boundVariablesAndComponents.slice(0, definition.numberOfBoundVariables);
      const components = boundVariablesAndComponents.slice(definition.numberOfBoundVariables);
      return new DefinedExpression(definition, boundVariableNames, components.map(Parser.parseExpression));
    } else if (_.isArray(rawExpression) && _.isNumber(rawExpression[0])) { // Function parameter
      const [level, index] = rawExpression;
      return new FunctionParameter(level, index);
    } else if (_.isObject(rawExpression)) { // Application
      let [[name, args]] = _.toPairs(rawExpression);
      return new ExpressionApplication(name, args.map(Parser.parseExpression));
    } else {
      throw `Unrecognised expression ${JSON.stringify(rawExpression)}`
    }
  }
  static parseInferenceSummary(inference) {
    inference.premises = inference.premises.map(Parser.parseExpression);
    inference.conclusion = Parser.parseExpression(inference.conclusion);
  }
  static parsePremise(premise) {
    premise.statement && (premise.statement = Parser.parseExpression(premise.statement));
    premise.premises && _.each(premise.premises, Parser.parsePremise);
  }
  static parseStep(step) {
    step.assumption && (step.assumption = Parser.parseExpression(step.assumption));
    step.statement && (step.statement = Parser.parseExpression(step.statement));
    step.provenStatement && (step.provenStatement = Parser.parseExpression(step.provenStatement));
    step.inference && Parser.parseInferenceSummary(step.inference);
    step.inferenceApplication && Parser.parseInferenceSummary(step.inferenceApplication.inference);
    step.substeps && _.each(step.substeps, Parser.parseStep);
    step.premises && _.each(step.premises, Parser.parsePremise);
  }
  static parseTheorem(rawTheorem) {
    const theorem = Parser.parseInference(rawTheorem);
    _.each(theorem.proof, Parser.parseStep);
    return theorem;
  }
  static parseInference(rawInference) {
    const inference = _.cloneDeep(rawInference);
    inference.premises && (inference.premises = inference.premises.map(Parser.parseExpression));
    inference.conclusion && (inference.conclusion = Parser.parseExpression(inference.conclusion));
    return inference;
  }
  static parseSubstitutions(substitutions) {
    return substitutions.map(substitution => _.mapValues(substitution, type => _.mapValues(type, e => e && Parser.parseExpression(e))))
  }
  static parseInferenceSuggestions(suggestions) {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      Parser.parseInferenceSummary(suggestion.inference);
      suggestion.substitutions = Parser.parseSubstitutions(suggestion.substitutions);
      return suggestion;
    })
  }
  static parsePremiseSuggestions(suggestionsForPremises) {
    return suggestionsForPremises.map(suggestionsForPremise =>
      suggestionsForPremise.map(suggestionJson => {
        const suggestion = _.cloneDeep(suggestionJson);
        suggestion.statement && (suggestion.statement = Parser.parseExpression(suggestion.statement));
        suggestion.substitutions = Parser.parseSubstitutions(suggestion.substitutions);
        return suggestion;
      })
    );
  }
  static parseEntries(entriesJson) {
    return entriesJson.map(entryJson => {
      const entry = _.cloneDeep(entryJson);
      entry.premises && (entry.premises = entry.premises.map(Parser.parseExpression));
      entry.conclusion && (entry.conclusion = Parser.parseExpression(entry.conclusion));
      entry.defaultValue && (entry.defaultValue = Parser.parseExpression(entry.defaultValue));
      entry.definingStatement && (entry.definingStatement = Parser.parseExpression(entry.definingStatement));
      return entry;
    });
  }
}
