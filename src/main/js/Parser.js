import _ from "lodash";
import {
  DefinedExpression,
  Variable,
  FunctionParameter,
  PropertyExpression,
  TypeExpression,
  StandalonePropertyExpression, TypeQualifierExpression, RelatedObjectExpression, TypeRelationExpression,
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

function tokenize(str)  {
  function splitToken(word) {
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

export class Parser {
  constructor(definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions) {
    this.definitions = definitions;
    this.typeDefinitions = typeDefinitions;
    this.typeRelationDefinitions = typeRelationDefinitions;
    this.standalonePropertyDefinitions = standalonePropertyDefinitions;
    this.stepCounter = 0;
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
        const qualifierAndParentType = _.chain(self.typeDefinitions).map(d => { return {qualifierDefinition: _.find(d.qualifiers, q => q.qualifiedSymbol === firstToken), parentType: d}}).filter("qualifierDefinition").find().value();
        const propertyAndParentType = _.chain(self.typeDefinitions).map(d => { return {property: _.find(d.properties, p => p.qualifiedSymbol === firstToken), parentType: d}}).filter("property").find().value();
        const objectAndParentType = _.chain(self.typeDefinitions).map(d => { return {object: _.find(d.relatedObjects, o => o.qualifiedSymbol === firstToken), parentType: d}}).filter("object").find().value();
        const typeRelationDefinition = self.typeRelationDefinitions[firstToken];
        const standalonePropertyDefinition = self.standalonePropertyDefinitions[firstToken];
        if (expressionDefinition) {
          const boundVariables = tokensAfterFirst.slice(0, expressionDefinition.numberOfBoundVariables);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterFirst.slice(expressionDefinition.numberOfBoundVariables), expressionDefinition.components.length);
          if (_.includes(expressionDefinition.attributes, "conjunction")) {
            const [firstComponent, secondComponent] = components;
            if (firstComponent instanceof TypeExpression && secondComponent instanceof TypeQualifierExpression && _.includes(firstComponent.definition.qualifiers, secondComponent.definition)) {
              if (secondComponent.term.serialize() === firstComponent.term.serialize() && !firstComponent.definition.defaultQualifier) {
                firstComponent.addQualifier(secondComponent.definition, secondComponent.qualifierComponents, expressionDefinition);
                return [firstComponent, tokensAfterComponents];
              }
            }
            if (firstComponent instanceof TypeExpression && secondComponent instanceof PropertyExpression && _.includes(firstComponent.definition.properties, secondComponent.definition)) {
              function matchQualifiers() {
                if (!firstComponent.definition.defaultQualifier && !secondComponent.definition.requiredParentQualifier) {
                  return true
                } else if (firstComponent.definition.defaultQualifier || (secondComponent.definition.requiredParentQualifier && firstComponent.explicitQualifier && firstComponent.explicitQualifier.symbol === secondComponent.definition.requiredParentQualifier)) {
                  return secondComponent.qualifierComponents.map(c => c.serialize()).join(" ") === firstComponent.qualifierComponents.map(c => c.serialize()).join(" ")
                } else {
                  return false;
                }
              }
              if (secondComponent.term.serialize() === firstComponent.term.serialize() && matchQualifiers()) {
                firstComponent.addProperty(secondComponent.definition, expressionDefinition);
                return [firstComponent, tokensAfterComponents];
              }
            }
            if (firstComponent instanceof TypeExpression && secondComponent instanceof RelatedObjectExpression && _.includes(firstComponent.definition.relatedObjects, secondComponent.definition)) {
              function matchQualifiers() {
                if (!firstComponent.definition.defaultQualifier && !secondComponent.definition.requiredParentQualifier) {
                  return true
                } else if (firstComponent.definition.defaultQualifier || (secondComponent.definition.requiredParentQualifier && firstComponent.explicitQualifier && firstComponent.explicitQualifier.symbol === secondComponent.definition.requiredParentQualifier)) {
                  return secondComponent.qualifierComponents.map(c => c.serialize()).join(" ") === firstComponent.qualifierComponents.map(c => c.serialize()).join(" ")
                } else {
                  return false;
                }
              }
              if (secondComponent.parentTerm.serialize() === firstComponent.term.serialize() && matchQualifiers()) {
                firstComponent.addObject(secondComponent.definition, secondComponent.term, expressionDefinition);
                return [firstComponent, tokensAfterComponents];
              }
            }
          }
          return [new DefinedExpression(expressionDefinition, boundVariables, components), tokensAfterComponents];
        } else if (typeDefinition) {
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterTerm, typeDefinition.defaultQualifier?.variableDefinitions.length ?? 0);
          return [new TypeExpression(typeDefinition, term, null, components, [], [], null), tokensAfterComponents];
        } else if (qualifierAndParentType) {
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterTerm, qualifierAndParentType.qualifierDefinition.qualifier.variableDefinitions.length);
          return [new TypeQualifierExpression(qualifierAndParentType.qualifierDefinition, qualifierAndParentType.parentType, term, components), tokensAfterComponents];
        } else if (propertyAndParentType) {
          const {property, parentType} = propertyAndParentType;
          const requiredParentQualifier = property.requiredParentQualifier ? _.find(parentType.qualifiers, q => q.symbol === property.requiredParentQualifier) : null;
          const qualifier = requiredParentQualifier?.qualifier || parentType.defaultQualifier;
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterTerm, qualifier?.variableDefinitions.length ?? 0);
          return [new PropertyExpression(property, parentType, term, components), tokensAfterComponents];
        } else if (objectAndParentType) {
          const {object, parentType} = objectAndParentType;
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [parentTerm, tokensAfterParentTerm] = parseExpressionFromTokens(tokensAfterTerm);
          const requiredParentQualifier = object.requiredParentQualifier ? _.find(parentType.qualifiers, q => q.symbol === object.requiredParentQualifier) : null;
          const qualifier = requiredParentQualifier?.qualifier || parentType.defaultQualifier;
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterParentTerm, qualifier?.variableDefinitions.length ?? 0);
          return [new RelatedObjectExpression(object, parentType, term, parentTerm, components), tokensAfterComponents];
        } else if (typeRelationDefinition) {
          const [firstTerm, tokensAfterFirstTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [secondTerm, tokensAfterSecondTerm] = parseExpressionFromTokens(tokensAfterFirstTerm);
          return [new TypeRelationExpression(typeRelationDefinition, firstTerm, secondTerm), tokensAfterSecondTerm];
        }else if (standalonePropertyDefinition) {
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterTerm, standalonePropertyDefinition.numberOfComponents);
          return [new StandalonePropertyExpression(standalonePropertyDefinition, term, components), tokensAfterComponents];
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
    const [expression, tokens] = parseExpressionFromTokens(tokenize(json));
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
          ++this.stepCounter,
          this.parseExpression(stepJson.statement),
          stepJson.premises.map(this.parsePremise),
          this.parseInferenceWithSummary(stepJson.inference, inferenceSummaries),
          stepJson.referencedLinesForAssertion.map(this.parseReference));
      case "deduction":
        return new DeductionStep(
          ++this.stepCounter,
          this.parseExpression(stepJson.assumption),
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          this.definitions[stepJson.deductionDefinition]);
      case "generalization":
        return new GeneralizationStep(
          ++this.stepCounter,
          stepJson.variableName,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          this.definitions[stepJson.generalizationDefinition]);
      case "naming":
        return new NamingStep(
          ++this.stepCounter,
          stepJson.variableName,
          this.parseExpression(stepJson.assumption),
          this.parseExpression(stepJson.statement),
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          this.parseInferenceWithSummary(stepJson.inference, inferenceSummaries),
          stepJson.referencedLinesForExtraction.map(this.parseReference));
      case "target":
        return new TargetStep(
          ++this.stepCounter,
          this.parseExpression(stepJson.statement));
      case "elided":
        return new ElidedStep(
          ++this.stepCounter,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          stepJson.highlightedInference && this.parseInferenceWithSummary(stepJson.highlightedInference, inferenceSummaries),
          stepJson.description);
      case "subproof":
        return new SubproofStep(
          ++this.stepCounter,
          stepJson.name,
          this.parseSteps(stepJson.substeps, inferenceSummaries));
      case "existingStatementExtraction":
        return new ElidedStep(
          ++this.stepCounter,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          null,
          "Extraction from previous step");
      case "wrappedPremiseDerivation":
        return new ElidedStep(
          ++this.stepCounter,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          null,
          "Premise derivation");
      case "inferenceExtraction":
      case "wrappedInferenceApplication":
      case "inferenceWithPremiseDerivations": {
        const substeps = this.parseSteps(stepJson.substeps, inferenceSummaries);
        return new ElidedStep(
          ++this.stepCounter,
          substeps,
          this.parseInferenceWithSummary(stepJson.inference, inferenceSummaries),
          null);
      }
      default:
        throw "Unrecognised step " + JSON.stringify(stepJson);
    }
  };
  
  parseSteps = (json, inferenceSummaries) => {
    return json.map(stepJson => this.parseStep(stepJson, inferenceSummaries));
  };
  parseStepsWithReferenceChanges = (json, inferenceSummaries) => {
    return json.map(({step: stepJson, path}) => {return {step: this.parseStep(stepJson, inferenceSummaries), path}});
  };


  parseTheorem = (theoremJson, inferences) => {
    return new Theorem(
      theoremJson.name,
      theoremJson.id,
      theoremJson.key,
      theoremJson.variableDefinitions,
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
    definition.definitionPredicate && (definition.definitionPredicate = this.parseExpression(definition.definitionPredicate));
    definition.defaultValue = this.parseExpression(definition.defaultValue);
    definition.premises && (definition.premises = definition.premises.map(this.parseExpression));
    _.forEach(definition.disambiguatorAdders, da => da.template = this.parseExpression(da.template));
    return definition;
  };
  parseDefinitionWithDefiningStatement = (definitionJson) => {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement = this.parseExpression(definition.definingStatement);
    return definition;
  };
  parseSubstitutions = (substitutions) => {
    const statements = _.map(substitutions.statements, s => s && this.parseExpression(s));
    const terms = _.map(substitutions.terms, t => t && this.parseExpression(t));
    const statementApplications = _.mapValues(substitutions.statementApplications, ss => _.map(ss, this.parseExpression));
    const termApplications = _.mapValues(substitutions.termApplications, ts => _.map(ts, this.parseExpression));
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
      suggestion.reference && (suggestion.reference = this.parseReference(suggestion.reference));
      return suggestion;
    })
  };
  parseInferenceRewriteSuggestions = (suggestions) => {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      this.parseInferenceSummary(suggestion.inference);
      suggestion.source = this.parseExpression(suggestion.source);
      suggestion.result = this.parseExpression(suggestion.result);
      return suggestion;
    })
  };
  parseEntries = (entryWrappers) => {
    return entryWrappers.map(entryWrapper => {
      entryWrapper = _.cloneDeep(entryWrapper);
      let entry = entryWrapper.entry;
      entry?.premises && (entry.premises = entry.premises.map(this.parseExpression));
      entry?.conclusion && (entry.conclusion = this.parseExpression(entry.conclusion));
      entry?.defaultValue && (entry.defaultValue = this.parseExpression(entry.defaultValue));
      entry?.definingStatement && (entry.definingStatement = this.parseExpression(entry.definingStatement));
      return entryWrapper;
    });
  };
  parseDisplayShorthand = (json) => {
    const shorthand = _.cloneDeep(json);
    shorthand.template = this.parseExpression(shorthand.template);
    return shorthand;
  };
  parseBinaryRelation = (json) => {
    const relation = _.cloneDeep(json);
    relation.template = this.parseExpression(relation.template);
    return relation;
  };
  parseDisambiguatorAdder = (json) => {
    const disambiguatorAdder = _.cloneDeep(json);
    disambiguatorAdder.template = this.parseExpression(disambiguatorAdder.template);
    return disambiguatorAdder;
  };
}
