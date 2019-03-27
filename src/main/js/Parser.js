import _ from "lodash";
import {Expression} from "./models/Expression";


export class Parser {
  static parseInferenceSummary(inference) {
    inference.premises = inference.premises.map(Expression.parseFromJson);
    inference.conclusion = Expression.parseFromJson(inference.conclusion);
  }
  static parsePremise(premise) {
    premise.statement && (premise.statement = Expression.parseFromJson(premise.statement));
    premise.premises && _.each(premise.premises, Parser.parsePremise);
  }
  static parseStep(step) {
    step.assumption && (step.assumption = Expression.parseFromJson(step.assumption));
    step.statement && (step.statement = Expression.parseFromJson(step.statement));
    step.provenStatement && (step.provenStatement = Expression.parseFromJson(step.provenStatement));
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
    inference.premises && (inference.premises = inference.premises.map(Expression.parseFromJson));
    inference.conclusion && (inference.conclusion = Expression.parseFromJson(inference.conclusion));
    return inference;
  }
  static parseSubstitutions(substitutions) {
    return substitutions.map(substitution => _.mapValues(substitution, type => _.mapValues(type, e => e && Expression.parseFromJson(e))))
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
        suggestion.statement && (suggestion.statement = Expression.parseFromJson(suggestion.statement));
        suggestion.substitutions = Parser.parseSubstitutions(suggestion.substitutions);
        return suggestion;
      })
    );
  }
  static parseEntries(entriesJson) {
    return entriesJson.map(entryJson => {
      const entry = _.cloneDeep(entryJson);
      entry.premises && (entry.premises = entry.premises.map(Expression.parseFromJson));
      entry.conclusion && (entry.conclusion = Expression.parseFromJson(entry.conclusion));
      entry.defaultValue && (entry.defaultValue = Expression.parseFromJson(entry.defaultValue));
      entry.definingStatement && (entry.definingStatement = Expression.parseFromJson(entry.definingStatement));
      return entry;
    });
  }
}
