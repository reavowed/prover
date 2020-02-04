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
  ScopedVariableStep,
  StepReference,
  SubproofStep,
  TargetStep
} from "./models/Step";

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
    if (_.isArray(json) && _.isString(json[0])) {
      const [definitionSymbol, ...boundVariablesAndComponents] = json;
      const expressionDefinition = this.definitions[definitionSymbol];
      const typeDefinition = this.typeDefinitions[definitionSymbol];
      const parentTypeDefinition = _.find(this.typeDefinitions, d => _.has(d.properties, definitionSymbol));
      if (expressionDefinition) {
        const boundVariableNames = boundVariablesAndComponents.slice(0, expressionDefinition.numberOfBoundVariables);
        const componentsJson = boundVariablesAndComponents.slice(expressionDefinition.numberOfBoundVariables);
        if (_.includes(expressionDefinition.attributes, "conjunction")) {
          const [firstComponentJson, secondComponentJson] = componentsJson;
          const firstComponent = this.parseExpression(firstComponentJson);
          if (firstComponent instanceof TypeExpression && _.isArray(secondComponentJson) && secondComponentJson.length > 0 && _.includes(firstComponent.definition.properties, secondComponentJson[0])) {
            const [propertyName, termJson, ...otherComponentsJson] = secondComponentJson;
            const property = firstComponent.definition.properties[propertyName];
            const term = this.parseExpression(termJson);
            const otherComponents = otherComponentsJson.map(this.parseExpression);
            if (term.serialize() === firstComponent.term.serialize() && otherComponents.map(c => c.serialize()).join(" ") === firstComponent.otherComponents.map(c => c.serialize()).join(" ")) {
              firstComponent.addProperty(property);
              return firstComponent;
            }
          }
        }
        return new DefinedExpression(expressionDefinition, boundVariableNames, componentsJson.map(this.parseExpression));
      } else if (typeDefinition) {
        return new TypeExpression(typeDefinition, this.parseExpression(boundVariablesAndComponents[0]), boundVariablesAndComponents.slice(1).map(this.parseExpression), [])
      } else if (parentTypeDefinition) {
        return new PropertyExpression(parentTypeDefinition, definitionSymbol, parentTypeDefinition.properties[definitionSymbol], this.parseExpression(boundVariablesAndComponents[0]), boundVariablesAndComponents.slice(1).map(this.parseExpression))
      } else {
        throw `Unrecognised definition ${definitionSymbol}`
      }
    } else if (_.isArray(json) && _.isNumber(json[0])) {
      const [level, index] = json;
      return new FunctionParameter(level, index);
    } else if (_.isObject(json)) {
      let [[name, args]] = _.toPairs(json);
      return new Variable(name, args.map(this.parseExpression));
    } else {
      throw `Unrecognised expression ${JSON.stringify(json)}`
    }
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
  
  parseSteps = (json, inferenceSummaries) => {
    return json.map(stepJson => {
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
        case "scopedVariable":
          return new ScopedVariableStep(
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
    });
  };

  parseTheorem = (theoremJson, inferences) => {
    return {
      name: theoremJson.name,
      id: theoremJson.id,
      key: theoremJson.key,
      premises: theoremJson.premises.map(this.parseExpression),
      conclusion: this.parseExpression(theoremJson.conclusion),
      proofs: theoremJson.proofs.map(proof => this.parseSteps(proof.steps, inferences))
    };
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
      this.parsePossibleConclusions(possibleInference.possibleConclusions);
      return possibleInference;
    });
    return possibleInferences;
  };
  parsePossibleConclusions = (possibleConclusions) => {
    _.forEach(possibleConclusions, c => {
      c.conclusion = this.parseExpression(c.conclusion);
      c.substitutions = c.substitutions && this.parseSubstitutions(c.substitutions);
      _.forEach(c.possiblePremises, p => {
        p.premise = this.parseExpression(p.premise);
        _.forEach(p.possibleMatches, m => {
          m.matchingPremise = this.parseExpression(m.matchingPremise);
          m.substitutions = this.parseSubstitutions(m.substitutions);
        });
      });
    });
    return possibleConclusions;
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
