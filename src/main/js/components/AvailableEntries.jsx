import * as React from "react";
import {Parser} from "../Parser";

const AvailableEntries = React.createContext();
AvailableEntries.create = function(parser, definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences = [], binaryRelations = []) {
  return {
    parser,
    definitions,
    typeDefinitions,
    typeRelationDefinitions,
    standalonePropertyDefinitions,
    definitionShorthands,
    displayShorthands: displayShorthands.map(parser.parseDisplayShorthand),
    inferences,
    binaryRelations: binaryRelations.map(parser.parseBinaryRelation),
    disambiguatorAdders: _.flatMap(definitions, d => d.disambiguatorAdders).map(parser.parseDisambiguatorAdder)
  };
};
AvailableEntries.fromEntryProps = function({definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences = [], binaryRelations = []}) {
  const parser = new Parser(definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions);
  return [parser, AvailableEntries.create(parser, definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations)];
};

export default AvailableEntries;
