import * as React from "react";
import {Parser} from "../Parser";
import {
    DisplayShorthand,
    ExpressionDefinitionSummary,
    InferenceSummary,
    StandalonePropertyDefinitionSummary,
    TypeDefinitionSummary,
    TypeRelationDefinitionSummary
} from "./definitions/EntryDefinitionSummaries";
import {DisambiguatedSymbol, DisambiguatorAdder} from "./definitions/DefinitionParts";
import _ from "lodash";
import {BinaryRelation} from "./definitions/BinaryRelation";

export type AvailableEntries = {
  parser: Parser
  definitions: {[key: string]: ExpressionDefinitionSummary},
  typeDefinitions: {[key: string]: TypeDefinitionSummary},
  typeRelationDefinitions: {[key: string]: TypeRelationDefinitionSummary},
  standalonePropertyDefinitions: {[key: string]: StandalonePropertyDefinitionSummary}
  definitionShorthands: {[key: string]: DisambiguatedSymbol}
  displayShorthands: DisplayShorthand[]
  inferences: InferenceSummary[]
  binaryRelations: BinaryRelation[]
  disambiguatorAdders: DisambiguatorAdder[]
}

const AvailableEntriesContext = React.createContext<AvailableEntries>({} as AvailableEntries);

type AvailableEntriesProps = {
    definitions: {[key: string]: ExpressionDefinitionSummary}
    typeDefinitions: {[key: string]: TypeDefinitionSummary}
    typeRelationDefinitions: {[key: string]: TypeRelationDefinitionSummary}
    standalonePropertyDefinitions: {[key: string]: StandalonePropertyDefinitionSummary}
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
