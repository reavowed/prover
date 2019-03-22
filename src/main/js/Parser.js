import _ from "lodash";

function formatWithReplacement(text, regex, handlePlain, handleMatch) {
  const matches = text.matchAll(regex);
  let indexOfLastMatchEnd = 0;
  let html = "";
  for (const match of matches) {
    html += handlePlain(text.substr(indexOfLastMatchEnd, match.index - indexOfLastMatchEnd));
    html += handleMatch(match);
    indexOfLastMatchEnd = match.index + match[0].length;
  }
  html += handlePlain(text.substr(indexOfLastMatchEnd));
  return html;
}

export function formatHtml(text, replacementFunction) {
  if (!replacementFunction) {
    replacementFunction = x => x;
  }
  return formatWithReplacement(text, /([_^])([^\s)}]+)/g, replacementFunction, match => {
    if (match[1] === "_") {
      return `<sub>${match[2]}</sub>`
    } else if (match[1] === "^") {
      return `<sup>${match[2]}</sup>`
    }
  });
}

function replacePlaceholders(text, components) {
  return formatWithReplacement(text, /%(\d+)/g, x => x,  match => {
    const index = parseInt(match[1]);
    return components[index];
  });
}

function matchShorthand(template, expression, boundVariableLists) {
  if (_.isString(template)) {
    return expression.toHtml(boundVariableLists, false);
  } else if (_.isArray(template) && _.isString(template[0])) {
    if ((expression instanceof DefinedExpression) && (expression.definition.symbol === template[0])) {
      let innerBoundVariableLists = expression.boundVariableNames.length > 0 ? [expression.boundVariableNames, ...boundVariableLists] : boundVariableLists;
      const componentMatches = _.zipWith(
        template.slice(1 + expression.definition.numberOfBoundVariables),
        expression.components,
        (t, c) => matchShorthand(t, c, innerBoundVariableLists));
      if (_.every(componentMatches)) {
        return [...expression.boundVariableNames, ..._.flatten(componentMatches)];
      }
    }
  } else if (_.isArray(template) && _.isNumber(template[0])) {
    if ((expression instanceof FunctionParameter) && _.isEqual(template, [expression.level, expression.index])) {
      return [];
    }
  }
}

export class VariableOrConstant {
  constructor(name) {
    this.name = name;
  }
  serialize() {
    return this.name;
  }
  toHtml() {
    return formatHtml(this.name);
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
  toHtml(boundVariableLists, safe) {
    for (let shorthand of window.shorthands) {
      const matches = matchShorthand(shorthand.template, this, boundVariableLists);
      if (matches) {
        let formatString = (safe && shorthand.requiresBrackets) ?
          "(" + shorthand.baseFormatString + ")" :
          shorthand.baseFormatString;
        return formatHtml(formatString, s => replacePlaceholders(s, matches));
      }
    }

    let formatString = (safe && this.definition.requiresBrackets) ?
      "(" + this.definition.baseFormatString + ")" :
      this.definition.baseFormatString;
    let innerBoundVariableLists = this.boundVariableNames.length > 0 ? [this.boundVariableNames, ...boundVariableLists] : boundVariableLists;
    let componentsHtml = this.components.map(c => c.toHtml(innerBoundVariableLists, true));
    return formatHtml(formatString, s => replacePlaceholders(s, [...this.boundVariableNames, ...componentsHtml]));
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
  toHtml(boundVariableLists) {
    return boundVariableLists[this.level][this.index];
  }
  textForHtml(boundVariableLists) {
    return boundVariableLists[this.level][this.index];
  }
}
export class ExpressionApplication {
  constructor(name, components) {
    this.name = name;
    this.components = components;
  }
  serialize() {
    return `with ${this.name} (${_.map(this.components, a => a.serialize())})`
  }
  toHtml(boundVariableLists) {
    const formatString = this.name + "(" + this.components.map((_, i) => "%" + i).join(", ") + ")";
    let argsHtml = this.components.map(c => c.toHtml(boundVariableLists, true));
    return formatHtml(formatString, s => replacePlaceholders(s, argsHtml));
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
  static parseInference(inference) {
    inference.premises = inference.premises.map(Parser.parseExpression);
    inference.conclusion = Parser.parseExpression(inference.conclusion);
  }
  static parseStep(step) {
    step.assumption && (step.assumption = Parser.parseExpression(step.assumption));
    step.statement && (step.statement = Parser.parseExpression(step.statement));
    step.provenStatement && (step.provenStatement = Parser.parseExpression(step.provenStatement));
    step.inference && Parser.parseInference(step.inference);
    step.inferenceApplication && Parser.parseInference(step.inferenceApplication.inference);
    step.substeps && _.each(step.substeps, Parser.parseStep);
  }
  static parseTheorem(rawTheorem) {
    const theorem = _.cloneDeep(rawTheorem);
    theorem.premises && (theorem.premises = theorem.premises.map(Parser.parseExpression));
    theorem.conclusion && (theorem.conclusion = Parser.parseExpression(theorem.conclusion));
    _.each(theorem.proof, Parser.parseStep);
    return theorem;
  }
  static parseInferenceSuggestions(rawSuggestionsJson) {
    return rawSuggestionsJson.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      Parser.parseInference(suggestion.inference);
      suggestion.substitutions = suggestion.substitutions.map(substitution => _.mapValues(substitution, type => _.mapValues(type, e => e && Parser.parseExpression(e))));
      return suggestion;
    })
  }
}
