import _ from "lodash";
import {
  DefinedExpression,
  Expression,
  FunctionParameter,
  PropertyExpression,
  RelatedObjectExpression,
  StandalonePropertyExpression,
  TypeExpression,
  TypeQualifierExpression,
  TypeRelationExpression,
  Variable,
} from "./models/Expression";
import {
  AssertionStep,
  DeductionStep,
  ElidedStep,
  GeneralizationStep,
  NamingStep,
  Step,
  SubproofStep,
  TargetStep
} from "./models/Step";
import {Theorem} from "./models/Theorem";

import {
  DisplayShorthand,
  ExpressionDefinitionSummary,
  InferenceSummary,
  PropertyDefinitionSummary,
  RelatedObjectDefinitionSummary,
  StandalonePropertyDefinitionSummary,
  TypeDefinitionSummary,
  TypeQualifierDefinitionSummary,
  TypeRelationDefinitionSummary
} from "./components/definitions/EntryDefinitionSummaries";
import {PremiseReference, StepReference} from "./components/definitions/Reference";
import {BinaryRelation} from "./components/definitions/BinaryRelation";
import {SerializedDisambiguatorAdder} from "./components/definitions/DefinitionParts";
import {Premise} from "./components/definitions/Premise";
import {Inference, InferenceWithSummary} from "./components/definitions/EntryDefinitions";

function tokenize(str: string): string[]  {
  function splitToken(word: string): string[] {
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
  private stepCounter: number = 0;
  private qualifiersWithParentTypes: {[key: string]: {qualifier: TypeQualifierDefinitionSummary, parentType: TypeDefinitionSummary}};
  private propertiesWithParentTypes: {[key: string]: {property: PropertyDefinitionSummary, parentType: TypeDefinitionSummary}};
  private objectsWithParentTypes: {[key: string]: {object: RelatedObjectDefinitionSummary, parentType: TypeDefinitionSummary}};
  constructor(
    private definitions: {[key: string]: ExpressionDefinitionSummary},
    private typeDefinitions: {[key: string]: TypeDefinitionSummary},
    private typeRelationDefinitions: {[key: string]: TypeRelationDefinitionSummary},
    private standalonePropertyDefinitions: {[key: string]: StandalonePropertyDefinitionSummary}
  ) {
    this.qualifiersWithParentTypes = _.fromPairs(
      _.flatMap(this.typeDefinitions, parentType =>
        _.map(parentType.qualifiers, qualifier =>
            [qualifier.qualifiedSymbol, {qualifier, parentType}])));
    this.propertiesWithParentTypes = _.fromPairs(
      _.flatMap(this.typeDefinitions, parentType =>
        _.map(parentType.properties, property =>
            [property.qualifiedSymbol, {property, parentType}])));
    this.objectsWithParentTypes = _.fromPairs(
      _.flatMap(this.typeDefinitions, parentType =>
        _.map(parentType.relatedObjects, object =>
            [object.qualifiedSymbol, {object, parentType}])));
  }
  get deductionDefinition() {
    return _.find(this.definitions, d => _.includes(d.attributes, "deduction"))!
  }
  get generalizationDefinition() {
    return _.find(this.definitions, d => _.includes(d.attributes, "generalization"))!
  }
  parseExpression = (json: string) => {
    const self = this;
    function parseExpressionsFromTokens(tokens: string[], numberOfExpressions: number): [Expression[], string[]] {
      return _.reduce<number, [Expression[], string[]]>(
        _.range(numberOfExpressions),
        ([componentsSoFar, tokens]) => {
          const [newComponent, newTokens] = parseExpressionFromTokens(tokens);
          return [[...componentsSoFar, newComponent], newTokens];
        },
        [[], tokens]
      );
    }
    function parseArgumentsFromTokens(tokens: string[]): [Expression[], string[]] {
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
    function matchQualifiers(firstComponent: TypeExpression, secondComponent: PropertyExpression | RelatedObjectExpression): boolean {
      if (!firstComponent.definition.defaultQualifier && !secondComponent.definition.requiredParentQualifier) {
        return true
      } else if (firstComponent.definition.defaultQualifier || (secondComponent.definition.requiredParentQualifier && firstComponent.explicitQualifier && firstComponent.explicitQualifier.symbol === secondComponent.definition.requiredParentQualifier)) {
        return secondComponent.qualifierComponents.map(c => c.serialize()).join(" ") === firstComponent.qualifierComponents.map(c => c.serialize()).join(" ")
      } else {
        return false;
      }
    }
    function parseExpressionFromTokens(tokens: string[]): [Expression, string[]] {
      if (tokens.length > 0) {
        const [firstToken, ...tokensAfterFirst] = tokens;
        const expressionDefinition = self.definitions[firstToken];
        const typeDefinition = self.typeDefinitions[firstToken];
        const qualifierAndParentType = self.qualifiersWithParentTypes[firstToken];
        const propertyAndParentType = self.propertiesWithParentTypes[firstToken];
        const objectAndParentType = self.objectsWithParentTypes[firstToken];
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
              if (secondComponent.term.serialize() === firstComponent.term.serialize() && matchQualifiers(firstComponent, secondComponent)) {
                firstComponent.addProperty(secondComponent.definition, expressionDefinition);
                return [firstComponent, tokensAfterComponents];
              }
            }
            if (firstComponent instanceof TypeExpression && secondComponent instanceof RelatedObjectExpression && _.includes(firstComponent.definition.relatedObjects, secondComponent.definition)) {
              if (secondComponent.parentTerm.serialize() === firstComponent.term.serialize() && matchQualifiers(firstComponent, secondComponent)) {
                firstComponent.addObject(secondComponent.definition, secondComponent.term, expressionDefinition);
                return [firstComponent, tokensAfterComponents];
              }
            }
          }
          return [new DefinedExpression(expressionDefinition, boundVariables, components), tokensAfterComponents];
        } else if (typeDefinition) {
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterTerm, typeDefinition.defaultQualifier?.variableDefinitions.length ?? 0);
          return [new TypeExpression(typeDefinition, term, undefined, components, [], [], undefined), tokensAfterComponents];
        } else if (qualifierAndParentType) {
          const [term, tokensAfterTerm] = parseExpressionFromTokens(tokensAfterFirst);
          const [components, tokensAfterComponents] = parseExpressionsFromTokens(tokensAfterTerm, qualifierAndParentType.qualifier.qualifier.variableDefinitions.length);
          return [new TypeQualifierExpression(qualifierAndParentType.qualifier, qualifierAndParentType.parentType, term, components), tokensAfterComponents];
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
          return [new StandalonePropertyExpression(standalonePropertyDefinition, term), tokensAfterTerm];
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

  parseReference = (json: any) => {
    if (_.isNumber(json.premiseIndex)) {
      return new PremiseReference(json.premiseIndex, json.internalPath || null);
    } else {
      return new StepReference(json.stepPath, json.suffix || null, json.internalPath || null);
    }
  };

  parseStep = (stepJson: any, inferenceSummaries: InferenceSummary[]): Step => {
    switch (stepJson.type) {
      case "Assertion":
        return new AssertionStep(
          ++this.stepCounter,
          this.parseExpression(stepJson.statement),
          stepJson.premises.map(this.parsePremise),
          this.parseInferenceWithSummary(stepJson.inference, inferenceSummaries),
          stepJson.path);
      case "Deduction":
        return new DeductionStep(
          ++this.stepCounter,
          this.parseExpression(stepJson.assumption),
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          this.deductionDefinition,
          stepJson.path);
      case "Generalization":
        return new GeneralizationStep(
          ++this.stepCounter,
          stepJson.variableName,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          this.generalizationDefinition,
          stepJson.path);
      case "Naming":
        return new NamingStep(
          ++this.stepCounter,
          stepJson.variableName,
          this.parseExpression(stepJson.assumption),
          this.parseExpression(stepJson.statement),
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          this.parseInferenceWithSummary(stepJson.inference, inferenceSummaries),
          stepJson.premises.map(this.parsePremise),
          stepJson.path);
      case "Target":
        return new TargetStep(
          ++this.stepCounter,
          this.parseExpression(stepJson.statement),
          stepJson.path);
      case "ElidedInference":
        return new ElidedStep(
          ++this.stepCounter,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          this.parseInferenceWithSummary(stepJson.inference, inferenceSummaries),
          null,
          stepJson.path);
      case "ElidedWithDescription":
        return new ElidedStep(
          ++this.stepCounter,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          null,
          stepJson.description,
          stepJson.path);
      case "Subproof":
        return new SubproofStep(
          ++this.stepCounter,
          stepJson.name,
          this.parseSteps(stepJson.substeps, inferenceSummaries),
          stepJson.path);
      default:
        throw "Unrecognised step " + JSON.stringify(stepJson);
    }
  };
  
  parseSteps = (json: any[], inferenceSummaries: InferenceSummary[]): Step[] => {
    return json.map(stepJson => this.parseStep(stepJson, inferenceSummaries));
  };

  parseTheorem = (theoremJson: any, inferences: InferenceSummary[]): Theorem => {
    return new Theorem(
      theoremJson.name,
      theoremJson.id,
      theoremJson.key,
      theoremJson.variableDefinitions,
      theoremJson.premises.map(this.parseExpression),
      this.parseExpression(theoremJson.conclusion),
      theoremJson.proofs.map((proof: any) => this.parseSteps(proof, inferences)));
  };

  parsePremise = (premiseJson: any): Premise => {
    switch(premiseJson.type) {
      case "pending":
        return {
          type: premiseJson.type,
          statement: this.parseExpression(premiseJson.statement)
        }
      case "given":
        return {
          type: premiseJson.type,
          statement: this.parseExpression(premiseJson.statement),
          referencedLine: this.parseReference(premiseJson.referencedLine)
        }
      case "simplification":
        return {
          type: premiseJson.type,
          statement: this.parseExpression(premiseJson.statement),
          premise: this.parsePremise(premiseJson.premise),
          referencedLine: this.parseReference(premiseJson.referencedLine),
          path: premiseJson.path
        }
      default:
        throw "Unknown premise type " + premiseJson.premiseType
    }
  };
  parseInference = (inferenceJson: any): Inference => {
    return {
      id: inferenceJson.id,
      name: inferenceJson.name,
      premises: inferenceJson.premises.map(this.parseExpression),
      conclusion: this.parseExpression(inferenceJson.conclusion),
      variableDefinitions: inferenceJson.variableDefinitions
    };
  };
  parseInferenceWithSummary = (inferenceJson: any, inferenceSummaries: InferenceSummary[]): InferenceWithSummary => {
    return {
      ...this.parseInference(inferenceJson),
      ...inferenceSummaries[inferenceJson.id]
    };
  };
  parseStatementDefinition = (definitionJson: any) => {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement && (definition.definingStatement = this.parseExpression(definition.definingStatement));
    definition.defaultValue = this.parseExpression(definition.defaultValue);
    return definition;
  };
  parseTermDefinition = (definitionJson: any) => {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement && (definition.definingStatement = this.parseExpression(definition.definingStatement));
    definition.definitionPredicate && (definition.definitionPredicate = this.parseExpression(definition.definitionPredicate));
    definition.defaultValue = this.parseExpression(definition.defaultValue);
    definition.premises && (definition.premises = definition.premises.map(this.parseExpression));
    _.forEach(definition.disambiguatorAdders, da => da.template = this.parseExpression(da.template));
    return definition;
  };
  parseDefinitionWithDefiningStatement = (definitionJson: any) => {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement = this.parseExpression(definition.definingStatement);
    return definition;
  };
  parseSubstitutions = (substitutions: any) => {
    const statements = _.map(substitutions.statements, s => s && this.parseExpression(s));
    const terms = _.map(substitutions.terms, t => t && this.parseExpression(t));
    const statementApplications = _.mapValues(substitutions.statementApplications, ss => _.map(ss, this.parseExpression));
    const termApplications = _.mapValues(substitutions.termApplications, ts => _.map(ts, this.parseExpression));
    return {statements, terms, statementApplications, termApplications};
  };
  parsePossibleInferences = (possibleInferences: any) => {
    _.forEach(possibleInferences, possibleInference => {
      possibleInference.inference = this.parseInference(possibleInference.inference);
      possibleInference.possibleTargets && this.parsePossibleTargets(possibleInference.possibleTargets);
      possibleInference.possibleConclusions && this.parsePossibleConclusions(possibleInference.possibleConclusions);
      return possibleInference;
    });
    return possibleInferences;
  };

  parsePossibleTargets = (possibleTargets: any) => {
    _.forEach(possibleTargets, t => {
      t.target = this.parseExpression(t.target);
      this.parsePossibleConclusions(t.possibleConclusions);
    });
  };
  parsePossibleConclusions = (possibleConclusions: any) => {
    _.forEach(possibleConclusions, this.parsePossibleConclusion);
    return possibleConclusions;
  };
  parsePossibleConclusion = (possibleConclusion: any) => {
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
  parsePremiseRewriteSuggestions = (suggestions: any[]) => {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      _.forEach(suggestion.rewriteSuggestions, s => s.result = this.parseExpression(s.result));
      suggestion.statement = this.parseExpression(suggestion.statement);
      suggestion.reference && (suggestion.reference = this.parseReference(suggestion.reference));
      return suggestion;
    })
  };
  parseInferenceRewriteSuggestions = (suggestions: any[]) => {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      suggestion.inference = this.parseInference(suggestion.inference);
      suggestion.source = this.parseExpression(suggestion.source);
      suggestion.result = this.parseExpression(suggestion.result);
      return suggestion;
    })
  };
  parseEntries = (entryWrappers: any[]) => {
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
  parseDisplayShorthand = (json: any): DisplayShorthand => {
    return {
      baseFormatString: json.baseFormatString,
      requiresBrackets: json.requiresBrackets,
      template: this.parseExpression(json.template),
      conditions: json.conditions
    }
  };
  parseBinaryRelation = (json: any): BinaryRelation => {
    const relation = _.cloneDeep(json);
    relation.template = this.parseExpression(relation.template);
    return relation;
  };
  parseDisambiguatorAdder = (json: SerializedDisambiguatorAdder) => {
    return {
      disambiguator: json.disambiguator,
      template: this.parseExpression(json.template)
    }
  };
}
