import _ from "lodash";
import {
  DefinedExpression,
  Variable,
  FunctionParameter,
  PropertyExpression,
  TypeExpression,
} from "./models/Expression";
import {
  AssertionStep,
  DeductionStep,
  ElidedStep,
  NamingStep,
  PremiseReference,
  GeneralizationStep,
  StepReference,
  SubproofStep,
  TargetStep
} from "./models/Step";
import {Theorem} from "./models/Theorem";

function serializeReference(reference) {
  if (!reference) return "???";
  const main = _.isNumber(reference.premiseIndex) ? "p" + reference.premiseIndex : reference.stepPath.join(".");
  const internalPath = reference.internalPath ? "-" + reference.internalPath.join(".") : "";
  const suffix = reference.suffix || "";
  return main + internalPath + suffix;
}

export class Parser {
  constructor( definitions, typeDefinitions) {
    this.definitions = definitions;
    this.typeDefinitions = typeDefinitions;
  }
  parseExpression = (json) => {
    const self = this;
    function parseExpressionsFromTokens(tokens, numberOfExpressions) {
      return _.reduce(
        _.range(numberOfExpressions),
        ([componentsSoFar, tokens]) => {
          const [newComponent, newTokens] = parseExpressionFromTokens(tokens);
          return [[...componentsSoFar, newComponent], newTokens];
        },
        [[], tokens]
      );
    }
    function parseArgumentsFromTokens(tokens) {
      if (tokens.length > 0) {
        if (tokens[0] === ")") {
          return [[], tokens.slice(1)];
        } else {
          const [currentArgument, tokensAfterCurrentArgument] = parseExpressionFromTokens(tokens);
          const [remainingArguments, remainingTokens] = parseArgumentsFromTokens(tokensAfterCurrentArgument);
          return [[currentArgument, ...remainingArguments], remainingTokens]
        }
      } else {
        throw "Expression ended unexpectedly during parsing"
      }
    }
    function parseExpressionFromTokens(tokens) {
      if (tokens.length > 0) {
        const [firstToken, ...tokensAfterFirst] = tokens;
        const expressionDefinition = self.definitions[firstToken];
        const typeDefinition = self.typeDefinitions[firstToken];
        const parentTypeDefinition = _.find(self.typeDefinitions, d => _.has(d.properties, firstToken));
        if (expressionDefinition) {
          const boundVariables = tokensAfterFirst.slice(0, expressionDefinition.numberOfBoundVariables);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterFirst.slice(expressionDefinition.numberOfBoundVariables), expressionDefinition.numberOfComponents);
          if (_.includes(expressionDefinition.attributes, "conjunction")) {
            const [firstComponent, secondComponent] = components;
            if (firstComponent instanceof TypeExpression && secondComponent instanceof DefinedExpression && _.includes(_.keys(firstComponent.definition.properties), secondComponent.definition.symbol)) {
              const property = firstComponent.definition.properties[secondComponent.definition.symbol];
              const [term, ...otherComponents] = secondComponent.components;
              if (term.serialize() === firstComponent.term.serialize() && otherComponents.map(c => c.serialize()).join(" ") === firstComponent.otherComponents.map(c => c.serialize()).join(" ")) {
                firstComponent.addProperty(property);
                return firstComponent;
              }
            }
          }
          return [new DefinedExpression(expressionDefinition, boundVariables, components), tokensAfterComponents];
        } else if (typeDefinition) {
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterTerm, typeDefinition.numberOfComponents);
          return [new TypeExpression(typeDefinition, term, components, []), tokensAfterComponents];
        } else if (parentTypeDefinition) {
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterTerm, parentTypeDefinition.numberOfComponents);
          return [new PropertyExpression(parentTypeDefinition, firstToken, parentTypeDefinition.properties[firstToken], term, components), tokensAfterComponents];
        } else if (firstToken === "with") {
          const [variableArguments, [name, ...remainingTokens]] = parseArgumentsFromTokens(tokensAfterFirst.slice(1));
          return [new Variable(name, variableArguments), remainingTokens];
        } else if (firstToken.startsWith("$")) {
          const match = firstToken.match(new RegExp("^(\\$+)(\\d+)"));
          if (!match) {
            throw `Invalid parameter ${firstToken}`
          }
          return [new FunctionParameter(match[1].length - 1, parseInt(match[2])), tokensAfterFirst]
        } else {
          return [new Variable(firstToken, []), tokensAfterFirst];
        }
      } else {
        throw "Expression ended unexpectedly during parsing"
      }
    }
    function splitToken(token) {
      const firstSingleCharacterIndex = _.findIndex(token, x => _.includes("(){}", x));
      if (firstSingleCharacterIndex === 0) {
        return [token[0], ...splitToken(token.substring(1))];
      } else if (firstSingleCharacterIndex > 0) {
        return [token.substring(0, firstSingleCharacterIndex), token[firstSingleCharacterIndex], ...splitToken(token.substring(firstSingleCharacterIndex + 1))];
      } else if (token.length) {
        return [token];
      } else {
        return [];
      }
    }
    const [expression, tokens] = parseExpressionFromTokens(_.flatMap(json.split(/ /), splitToken));
    if (tokens.length) {
      throw "Expression parsing ended before end of string";
    }
    return expression;
  };

  parseReference = (json) => {
    if (_.isNumber(json.premiseIndex)) {
      return new PremiseReference(json.premiseIndex, json.internalPath || null);
    } else {
      return new StepReference(json.stepPath, json.suffix || null, json.internalPath || null);
    }
  };

  parseInferenceWithSummary = (json, inferenceSummaries) => {
    const inference = this.parseInference(json);
    const summary = inferenceSummaries[inference.id];
    return {...inference, ...summary};
  };

  parseStep = (stepJson, inferenceSummaries) => {
    switch (stepJson.type) {
      case "assertion":
        return new AssertionStep(
          this.parseExpression(stepJson.statement),
          stepJson.premises.map(this.parsePremise),
          this.parseInferenceWithSummary(stepJson.inference, inferenceSummaries),
          stepJson.referencedLines.map(this.parseReference));
      case "deduction":
        return new DeductionStep(
          this.parseExpression(stepJson.assumption),
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          stepJson.provenStatement && this.parseExpression(stepJson.provenStatement));
      case "generalization":
        return new GeneralizationStep(
          stepJson.variableName,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          stepJson.provenStatement && this.parseExpression(stepJson.provenStatement));
      case "naming":
        return new NamingStep(
          stepJson.variableName,
          this.parseExpression(stepJson.assumption),
          this.parseExpression(stepJson.provenStatement),
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          this.parseInferenceWithSummary(stepJson.inference, inferenceSummaries),
          stepJson.referencedLines.map(this.parseReference),
          stepJson.referencedLinesForExtraction.map(this.parseReference));
      case "target":
        return new TargetStep(this.parseExpression(stepJson.statement));
      case "elided":
        return new ElidedStep(
          stepJson.provenStatement && this.parseExpression(stepJson.provenStatement),
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          stepJson.highlightedInference && this.parseInferenceWithSummary(stepJson.highlightedInference, inferenceSummaries),
          stepJson.description,
          stepJson.referencedLines.map(this.parseReference));
      case "subproof":
        return new SubproofStep(
          stepJson.name,
          stepJson.provenStatement && this.parseExpression(stepJson.provenStatement),
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          stepJson.referencedLines.map(this.parseReference));
      default:
        throw "Unrecognised step " + JSON.stringify(stepJson);
    }
  };
  
  parseSteps = (json, inferenceSummaries) => {
    return json.map(stepJson => this.parseStep(stepJson, inferenceSummaries));
  };

  parseTheorem = (theoremJson, inferences) => {
    return new Theorem(
      theoremJson.name,
      theoremJson.id,
      theoremJson.key,
      theoremJson.premises.map(this.parseExpression),
      this.parseExpression(theoremJson.conclusion),
      theoremJson.proofs.map(proof => this.parseSteps(proof.steps, inferences)));
  };

  parseInferenceSummary = (inference) => {
    inference.premises = inference.premises.map(this.parseExpression);
    inference.conclusion = this.parseExpression(inference.conclusion);
  };

  parsePremise = (premiseJson) => {
    const premise = _.cloneDeep(premiseJson);
    premiseJson.statement && (premise.statement = this.parseExpression(premiseJson.statement));
    premiseJson.premises && (premise.premises =  premise.premises.map(this.parsePremise));
    premiseJson.referencedLine && (premise.referencedLine = this.parseReference(premise.referencedLine));
    premise.serializedReference = serializeReference(premiseJson.referencedLine);
    return premise;
  };
  parseInference = (rawInference) => {
    const inference = _.cloneDeep(rawInference);
    inference.premises && (inference.premises = inference.premises.map(this.parseExpression));
    inference.conclusion && (inference.conclusion = this.parseExpression(inference.conclusion));
    return inference;
  };
  parseStatementDefinition = (definitionJson) => {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement && (definition.definingStatement = this.parseExpression(definition.definingStatement));
    definition.defaultValue = this.parseExpression(definition.defaultValue);
    return definition;
  };
  parseTermDefinition = (definitionJson) => {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement && (definition.definingStatement = this.parseExpression(definition.definingStatement));
    definition.defaultValue = this.parseExpression(definition.defaultValue);
    definition.premises && (definition.premises = definition.premises.map(this.parseExpression));
    return definition;
  };
  parseTypeDefinition = (definitionJson) => {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement && (definition.definingStatement = this.parseExpression(definition.definingStatement));
    return definition;
  };
  parseSubstitutions = (substitutions) => {
    const statements = _.mapValues(substitutions.statements, ([i, s]) => [i, this.parseExpression(s)]);
    const terms = _.mapValues(substitutions.terms, ([i, t]) => [i, this.parseExpression(t)]);
    const statementApplications = _.mapValues(substitutions.statementApplications, ([i, e]) => [i, e.map(this.parseExpression)]);
    const termApplications = _.mapValues(substitutions.termApplications, ([i, e]) => [i, e.map(this.parseExpression)]);
    return {statements, terms, statementApplications, termApplications};
  };
  parsePossibleInferences = (possibleInferences) => {
    _.forEach(possibleInferences, possibleInference => {
      this.parseInferenceSummary(possibleInference.inference);
      possibleInference.possibleTargets && this.parsePossibleTargets(possibleInference.possibleTargets);
      possibleInference.possibleConclusions && this.parsePossibleConclusions(possibleInference.possibleConclusions);
      return possibleInference;
    });
    return possibleInferences;
  };

  parsePossibleTargets = (possibleTargets) => {
    _.forEach(possibleTargets, t => {
      t.target = this.parseExpression(t.target);
      this.parsePossibleConclusions(t.possibleConclusions);
    });
  };
  parsePossibleConclusions = (possibleConclusions) => {
    _.forEach(possibleConclusions, this.parsePossibleConclusion);
    return possibleConclusions;
  };
  parsePossibleConclusion = (possibleConclusion) => {
    possibleConclusion.conclusion = this.parseExpression(possibleConclusion.conclusion);
    possibleConclusion.substitutions = possibleConclusion.substitutions && this.parseSubstitutions(possibleConclusion.substitutions);
    _.forEach(possibleConclusion.possiblePremises, p => {
      p.premise = this.parseExpression(p.premise);
      _.forEach(p.possibleMatches, m => {
        m.matchingPremise = this.parseExpression(m.matchingPremise);
        m.substitutions = this.parseSubstitutions(m.substitutions);
      });
    });
    return possibleConclusion;
  };
  parsePremiseRewriteSuggestions = (suggestions) => {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      _.forEach(suggestion.rewriteSuggestions, s => s.result = this.parseExpression(s.result));
      suggestion.statement = this.parseExpression(suggestion.statement);
      suggestion.reference = this.parseReference(suggestion.reference);
      return suggestion;
    })
  };
  parseInferenceRewriteSuggestions = (suggestions) => {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      this.parseInferenceSummary(suggestion.inference);
      suggestion.source = this.parseExpression(suggestion.source);
      suggestion.result = this.parseExpression(suggestion.result);
      suggestion.rewriteSuggestions = _.map(suggestion.rewriteSuggestions, s => { return { path: s.path, result: this.parseExpression(s.result) } });
      return suggestion;
    })
  };
  parseEntries = (entriesJson) => {
    return entriesJson.map(entryJson => {
      const entry = _.cloneDeep(entryJson);
      entry.premises && (entry.premises = entry.premises.map(this.parseExpression));
      entry.conclusion && (entry.conclusion = this.parseExpression(entry.conclusion));
      entry.defaultValue && (entry.defaultValue = this.parseExpression(entry.defaultValue));
      entry.definingStatement && (entry.definingStatement = this.parseExpression(entry.definingStatement));
      return entry;
    });
  };
}
