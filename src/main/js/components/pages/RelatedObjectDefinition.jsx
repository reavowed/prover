import React from "react";
import {Parser} from "../../Parser";
import AvailableEntriesContext, {createAvailableEntries} from "../AvailableEntriesContext";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableProperties from "./components/EditableProperties";
import {NavLinks} from "./components/NavLinks";
import RelatedObjectDefinitionDescription from "./components/RelatedObjectDefinitionDescription";
import StatementDefinitionUsages from "./components/StatementDefinitionUsages";
import {Page} from "./Page";
import {useMappedState} from "./utils/entryFunctions";

export function RelatedObjectDefinition(props) {
  const {definition: definitionJson, bookLink, chapterLink, url, previous, next, usages} = props;
  const availableEntries = createAvailableEntries(props);
  const [definition, setDefinition] = useMappedState(definitionJson, availableEntries.parser.parseDefinitionWithDefiningStatement);

  const editableProperties = [
    {label: "Symbol", initialValue: definition.symbol, endpointName: "symbol"},
    {label: "Explicit Name", initialValue: definition.explicitName, endpointName: "name"}
  ];

  return <AvailableEntriesContext.Provider value={availableEntries}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{definition.title.capitalize()}</h3>
      <RelatedObjectDefinitionDescription relatedObjectDefinition={definition} />
      <EditableProperties url={url} updateEntry={setDefinition} definitions={editableProperties} />
      <StatementDefinitionUsages usages={usages} statementDefinition={definition.statementDefinition} />
    </Page>
  </AvailableEntriesContext.Provider>;
}
