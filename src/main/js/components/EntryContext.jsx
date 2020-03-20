import * as React from "react";

const EntryContext = React.createContext();
EntryContext.create = function(parser, definitions, typeDefinitions, definitionShorthands, displayShorthands, inferences = [], binaryRelations = []) {
  return {
    parser,
    definitions,
    typeDefinitions,
    definitionShorthands,
    displayShorthands: displayShorthands.map(parser.parseDisplayShorthand),
    inferences,
    binaryRelations: binaryRelations.map(parser.parseBinaryRelation)
  };
};

export default EntryContext;
