import _ from "lodash";
import {Expression} from "./models/Expression";

function serializeReference(reference) {
  if (!reference) return "???";
  const main = _.isNumber(reference.premiseIndex) ? "p" + reference.premiseIndex : reference.stepPath.join(".");
  const internalPath = reference.internalPath ? "-" + reference.internalPath.join(".") : "";
  const suffix = reference.suffix || "";
  return main + internalPath + suffix;
}

export class Parser {
  static replaceShorthands = (event) => {
    const input = event.target;
    const initialText = input.value.substring(0, input.selectionStart);
    const finalText = input.value.substring(input.selectionStart);
    const replacedInitialText =_.reduce(_.toPairs(window.definitionShorthands), (text, [valueToReplace, symbol]) => {
      const regex = new RegExp('(?<=^|\\s)' + _.escapeRegExp(valueToReplace) + '(?=\\s$)', 'gim');
      return text.replace(regex, symbol);
    }, initialText);
    const replacedText = replacedInitialText + finalText;
    const callback = () => input.setSelectionRange(replacedInitialText.length, replacedInitialText.length);
    return [replacedText, callback];
  };
  static parseInferenceSummary(inference) {
    inference.premises = inference.premises.map(Expression.parseFromJson);
    inference.conclusion = Expression.parseFromJson(inference.conclusion);
  }

  static parsePremise(premiseJson) {
    const premise = _.cloneDeep(premiseJson);
    premiseJson.statement && (premise.statement = Expression.parseFromJson(premiseJson.statement));
    premiseJson.premises && (premise.premises =  premise.premises.map(Parser.parsePremise));
    premise.serializedReference = serializeReference(premiseJson.referencedLine);
    return premise;
  }
  static parseInference(rawInference) {
    const inference = _.cloneDeep(rawInference);
    inference.premises && (inference.premises = inference.premises.map(Expression.parseFromJson));
    inference.conclusion && (inference.conclusion = Expression.parseFromJson(inference.conclusion));
    return inference;
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

    const parseApplications = (applications, parser) => {
      const triples = _.map(_.toPairs(applications), ([nameAndCount, e]) => {
        const [_, name, count] = nameAndCount.match(/\((.*),(\d+)\)/);
        return [name, parseInt(count), parser(e)];
      });
      return Parser.doubleMapFromTriples(triples);
    };

    const statements = _.mapValues(substitutions.statements, s => s && Expression.parseFromJson(s));
    const terms = _.mapValues(substitutions.terms, t => t && Expression.parseFromJson(t));
    const predicates = parseApplications(substitutions.predicates, Expression.parseFromJson);
    const functions = parseApplications(substitutions.functions, Expression.parseFromJson);
    const predicateApplications = parseApplications(substitutions.predicateApplications, s => s.map(Expression.parseFromJson));
    const functionApplications = parseApplications(substitutions.functionApplications, s => s.map(Expression.parseFromJson));
    return {statements, terms, predicates, functions, predicateApplications, functionApplications};
  }
  static parseInferenceSuggestions(suggestions) {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      Parser.parseInferenceSummary(suggestion.inference);
      suggestion.substitutions && (suggestion.substitutions = _.map(suggestion.substitutions, Parser.parseSubstitutions));
      suggestion.conclusion = Expression.parseFromJson(suggestion.conclusion);
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
  static parsePremiseRewriteSuggestions(suggestions) {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      _.forEach(suggestion.rewriteSuggestions, s => s.result = Expression.parseFromJson(s.result));
      suggestion.statement = Expression.parseFromJson(suggestion.statement);
      return suggestion;
    })
  }
  static parseInferenceRewriteSuggestions(suggestions) {
    return suggestions.map(suggestionJson => {
      const suggestion = _.cloneDeep(suggestionJson);
      Parser.parseInferenceSummary(suggestion.inference);
      suggestion.source = Expression.parseFromJson(suggestion.source);
      suggestion.result = Expression.parseFromJson(suggestion.result);
      suggestion.rewriteSuggestions = _.map(suggestion.rewriteSuggestions, s => { return { path: s.path, result: Expression.parseFromJson(s.result) } });
      return suggestion;
    })
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
