import _ from "lodash";
import {Expression} from "./models/Expression";

export class Parser {
  static replaceShorthands = (text) => {
    _.each(_.toPairs(window.definitionShorthands), ([valueToReplace, symbol]) => {
      let regex = new RegExp('(?<=^|\\s)' + _.escapeRegExp(valueToReplace) + '(?=\\s)', 'gim');
      text = text.replace(regex, symbol);
    });
    return text;
  };
  static parseInferenceSummary(inference) {
    inference.premises = inference.premises.map(Expression.parseFromJson);
    inference.conclusion = Expression.parseFromJson(inference.conclusion);
  }
  static parsePremise(premiseJson) {
    const premise = _.cloneDeep(premiseJson);
    premiseJson.statement && (premise.statement = Expression.parseFromJson(premiseJson.statement));
    premiseJson.premises && (premise.premises =  premise.premises.map(Parser.parsePremise));
    return premise;
  }
  static parseInference(rawInference) {
    const inference = _.cloneDeep(rawInference);
    inference.premises && (inference.premises = inference.premises.map(Expression.parseFromJson));
    inference.conclusion && (inference.conclusion = Expression.parseFromJson(inference.conclusion));
    return inference;
  }
  static parseInferenceApplication(inferenceApplicationJson) {
    const inferenceApplication = _.cloneDeep(inferenceApplicationJson);
    inferenceApplication.inference = Parser.parseInference(inferenceApplication.inference);
    return inferenceApplication;
  }
  static parseStatementDefinition(definitionJson) {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement && (definition.definingStatement = Expression.parseFromJson(definition.definingStatement));
    definition.defaultValue = Expression.parseFromJson(definition.defaultValue);
    return definition;
  }
  static parseTermDefinition(definitionJson) {
    const definition = _.cloneDeep(definitionJson);
    definition.definingStatement && (definition.definingStatement = Expression.parseFromJson(definition.definingStatement));
    definition.defaultValue = Expression.parseFromJson(definition.defaultValue);
    definition.premises && (definition.premises = definition.premises.map(Expression.parseFromJson));
    return definition;
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
      suggestion.substitutions && (suggestion.substitutions = _.map(suggestion.substitutions, Parser.parseSubstitutions));
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
