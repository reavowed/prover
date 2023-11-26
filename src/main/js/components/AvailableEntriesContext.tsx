import * as React from "react";
import {Parser} from "../Parser";
import {
  DisplayShorthand,
  ExpressionDefinition,
  StandalonePropertyDefinition,
  TypeDefinition,
  TypeRelationDefinition
} from "./definitions/EntryDefinitions";
import {DisambiguatedSymbol, DisambiguatorAdder} from "./definitions/DefinitionParts";
import {InferenceSummary} from "./definitions/InferenceSummary";
import _ from "lodash";
import {BinaryRelation} from "./definitions/BinaryRelation";

type AvailableEntries = {
  parser: Parser
  definitions: {[key: string]: ExpressionDefinition},
  typeDefinitions: {[key: string]: TypeDefinition},
  typeRelationDefinitions: {[key: string]: TypeRelationDefinition},
  standalonePropertyDefinitions: {[key: string]: StandalonePropertyDefinition}
  definitionShorthands: {[key: string]: DisambiguatedSymbol}
  displayShorthands: DisplayShorthand[]
  inferences: InferenceSummary[]
  binaryRelations: BinaryRelation[]
  disambiguatorAdders: DisambiguatorAdder[]
}

const AvailableEntriesContext = React.createContext<AvailableEntries>({} as AvailableEntries);

type AvailableEntriesProps = {
    definitions: {[key: string]: ExpressionDefinition}
    typeDefinitions: {[key: string]: TypeDefinition}
    typeRelationDefinitions: {[key: string]: TypeRelationDefinition}
    standalonePropertyDefinitions: {[key: string]: StandalonePropertyDefinition}
    definitionShorthands: {[key: string]: DisambiguatedSymbol}
    displayShorthands: any[]
    inferences?: InferenceSummary[]
    binaryRelations?: any[]
}

export function createAvailableEntries({definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences = [], binaryRelations = []}: AvailableEntriesProps): AvailableEntries {
  const parser = new Parser(definitions, typeDefinitions, typeRelationDefinitions, standalonePropertyDefinitions);
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
}

export default AvailableEntriesContext;
