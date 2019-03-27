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
  static doubleMapFromTriples(triples) {
    const namesToPairLists = _.mapValues(_.groupBy(triples, 0), x => x.map(y => y.slice(1)));
    return _.mapValues(namesToPairLists, _.fromPairs);
  }
  static parseSubstitutions(substitutions) {
    const parseApplications = (applications) => {
      const triples = _.map(_.toPairs(applications), ([nameAndCount, e]) => {
        const [_, name, count] = nameAndCount.match(/\((.*),(\d+)\)/);
        return [name, parseInt(count), Expression.parseFromJson(e)];
      });
      return Parser.doubleMapFromTriples(triples);
    };

    const statements = _.mapValues(substitutions.statements, s => s && Expression.parseFromJson(s));
    const terms = _.mapValues(substitutions.terms, t => t && Expression.parseFromJson(t));
    const predicates = parseApplications(substitutions.predicates);
    const functions = parseApplications(substitutions.functions);
    return {statements, terms, predicates, functions};
  }
  static parseInferenceSuggestions(suggestions) {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      Parser.parseInferenceSummary(suggestion.inference);
      suggestion.substitutions = _.map(suggestion.substitutions, Parser.parseSubstitutions);
      return suggestion;
    })
  }
  static parsePremiseSuggestions(suggestionsForPremises) {
    return suggestionsForPremises.map(suggestionsForPremise =>
      suggestionsForPremise.map(suggestionJson => {
        const suggestion = _.cloneDeep(suggestionJson);
        suggestion.statement && (suggestion.statement = Expression.parseFromJson(suggestion.statement));
        suggestion.substitutions = _.map(suggestion.substitutions, Parser.parseSubstitutions);
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
