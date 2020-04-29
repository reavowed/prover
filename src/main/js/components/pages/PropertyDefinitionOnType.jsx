import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableExplicitName from "./components/EditableExplicitName";
import EditableSymbol from "./components/EditableSymbol";
import {NavLinks} from "./components/NavLinks";
import PropertyOnTypeDefinitionDescription from "./components/PropertyOnTypeDefinitionDescription";
import StatementDefinitionUsages from "./components/StatementDefinitionUsages";
import {Page} from "./Page";

export function PropertyDefinitionOnType({definition: definitionJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, bookLink, chapterLink, url, previous, next, usages}) {
  const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
  const definition = parser.parseDefinitionWithDefiningStatement(definitionJson);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);

  return <DisplayContext.Provider value={DisplayContext.forDefinitionWithDefiningStatement(definition, entryContext)}>
    <EntryContext.Provider value={entryContext}>
      <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
        <NavLinks previous={previous} next={next} />
        <h3>{definition.title.capitalize()}</h3>
        <PropertyOnTypeDefinitionDescription symbol={definition.symbol} parentTypeSymbol={definition.parentType.symbol} definingStatement={definition.definingStatement} />
        <hr/>
        <EditableSymbol symbol={definition.symbol} url={url} />
        <EditableExplicitName name={definition.explicitName} url={url} />
        <StatementDefinitionUsages usages={usages} statementDefinition={definition.statementDefinition} />
      </Page>
    </EntryContext.Provider>
  </DisplayContext.Provider>;
}
