import * as path from "path";
import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableExplicitName from "./components/EditableExplicitName";
import EditableProperty from "./components/EditableProperty";
import EditableSymbol from "./components/EditableSymbol";
import {NavLinks} from "./components/NavLinks";
import StatementDefinitionUsages from "./components/StatementDefinitionUsages";
import TypeDefinitionDescription from "./components/TypeDefinitionDescription";
import {Page} from "./Page";

export function TypeDefinition({definition: definitionJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, bookLink, chapterLink, url, previous, next, usages}) {
  const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);
  const definition = parser.parseDefinitionWithDefiningStatement(definitionJson);

  const saveFormat = (newFormat) => {
    return window.fetchJson(path.join(url, "format"), {method: "PUT", body: newFormat});
  };

  const serializedFormat = definition.qualifier?.format.originalValue ?
    "(" + definition.qualifier.format.originalValue + ")" + (definition.qualifier.format.requiresBrackets ? " requires-brackets" : "") + (definition.qualifier.format.requiresComponentBrackets ? "" : " no-component-brackets") :
    "";

  return <EntryContext.Provider value={entryContext}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{definition.title.capitalize()}</h3>
      <TypeDefinitionDescription symbol={definition.symbol} definingStatement={definition.definingStatement} />
      <hr/>
      <EditableSymbol symbol={definition.symbol} url={url} />
      <EditableExplicitName name={definition.explicitName} url={url} />
      <EditableProperty label="Main Term Name" initialValue={definition.defaultTermName} />
      {definition.qualifier && <EditableProperty label="Qualifier Term Names" initialValue={definition.qualifier.termNames.join(" ")} />}
      {definition.qualifier && <EditableProperty label="Qualifier Format" initialValue={serializedFormat} onSave={saveFormat} />}
      <StatementDefinitionUsages usages={usages} statementDefinition={definition.statementDefinition} />
    </Page>
  </EntryContext.Provider>;
}
