import React, {useMemo} from "react";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableProperties from "./components/EditableProperties";
import {NavLinks} from "./components/NavLinks";
import StatementDefinitionUsages from "./components/StatementDefinitionUsages";
import TypeDefinitionDescription from "./components/TypeDefinitionDescription";
import {Page} from "./Page";
import {useMappedState} from "./utils/entryFunctions";

export function TypeDefinition(props) {
  const {definition: definitionJson, bookLink, chapterLink, url, previous, next, usages} = props;
  const [parser, entryContext] = EntryContext.fromEntryProps(props);
  const [definition, setDefinition] = useMappedState(definitionJson, parser.parseDefinitionWithDefiningStatement);

  const serializedFormat = definition.qualifier?.format.originalValue ?
    "(" + definition.qualifier.format.originalValue + ")" + (definition.qualifier.format.requiresBrackets ? " requires-brackets" : "") + (definition.qualifier.format.requiresComponentBrackets ? "" : " no-component-brackets") :
    "";
  function serializeVariableDefinition(variableDefinition) {
    if (variableDefinition.attributes.length > 0) {
      return variableDefinition.name + " (" + variableDefinition.attributes.join(", ") + ")"
    } else {
      return variableDefinition.name
    }
  }

  const editableProperties = [
    {label: "Symbol", initialValue: definition.symbol, endpointName: "symbol"},
    {label: "Explicit Name", initialValue: definition.explicitName, endpointName: "name"},
    {label: "Main Variable", initialValue: serializeVariableDefinition(definition.mainVariableDefinition), endpointName: "mainVariableDefinition"},
    definition.qualifier && {label: "Qualifier Variables", initialValue: definition.qualifier.variableDefinitions.map(serializeVariableDefinition).join(" ")},
    definition.qualifier && {label: "Qualifier Format", initialValue: serializedFormat, endpointName: "format"},
    {
      label: "Definition",
      initialValue: definition.definingStatement.serializeNicely(
        [],
        {
          statements: [],
          terms: definition.allVariableDefinitions.map(
            ({name}) => {
              return {name, arity: 0}
            }
          )
        }),
      endpointName: "definingStatement"
    }
  ];


  return <EntryContext.Provider value={entryContext}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{definition.title.capitalize()}</h3>
      <TypeDefinitionDescription typeDefinition={definition} />
      <EditableProperties url={url} updateEntry={setDefinition} definitions={editableProperties} />
      <StatementDefinitionUsages usages={usages} statementDefinition={definition.statementDefinition} />
    </Page>
  </EntryContext.Provider>;
}
