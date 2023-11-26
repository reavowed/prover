import React from "react";
import {Parser} from "../../Parser";
import AvailableEntriesContext, {createAvailableEntries} from "../AvailableEntriesContext";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableProperties from "./components/EditableProperties";
import {NavLinks} from "./components/NavLinks";
import StatementDefinitionUsages from "./components/StatementDefinitionUsages";
import TypeQualifierDefinitionDescription from "./components/TypeQualifierDefinitionDescription";
import TypeRelationDefinitionDescription from "./components/TypeRelationDefinitionDescription";
import {Page} from "./Page";
import {serializeVariable, useMappedState} from "./utils/entryFunctions";

export function TypeRelationDefinition(props) {
  const {definition: definitionJson, bookLink, chapterLink, url, previous, next, usages} = props;
  const availableEntries = createAvailableEntries(props);
  const [definition, setDefinition] = useMappedState(definitionJson, availableEntries.parser.parseDefinitionWithDefiningStatement);

  const editableProperties = [
    {label: "Symbol", initialValue: definition.symbol, endpointName: "symbol"},
    {label: "Explicit Name", initialValue: definition.explicitName, endpointName: "name"},
    {label: "First Variable", initialValue: serializeVariable(definition.firstVariable), endpointName: "firstVariable"},
    {label: "Second Variable", initialValue: serializeVariable(definition.secondVariable), endpointName: "secondVariable"},
  ];


  return <AvailableEntriesContext.Provider value={availableEntries}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{definition.title.capitalize()}</h3>
      <TypeRelationDefinitionDescription typeRelationDefinition={definition} />
      <EditableProperties url={url} updateEntry={setDefinition} definitions={editableProperties} />
      <StatementDefinitionUsages usages={usages} statementDefinition={definition.statementDefinition} />
    </Page>
  </AvailableEntriesContext.Provider>;
}
