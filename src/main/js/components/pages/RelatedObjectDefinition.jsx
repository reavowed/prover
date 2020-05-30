import React from "react";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableExplicitName from "./components/EditableExplicitName";
import EditableSymbol from "./components/EditableSymbol";
import {NavLinks} from "./components/NavLinks";
import RelatedObjectDefinitionDescription from "./components/RelatedObjectDefinitionDescription";
import StatementDefinitionUsages from "./components/StatementDefinitionUsages";
import {Page} from "./Page";

export function RelatedObjectDefinition({definition: definitionJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, bookLink, chapterLink, url, previous, next, usages}) {
  const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
  const definition = parser.parseDefinitionWithDefiningStatement(definitionJson);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);

  return <EntryContext.Provider value={entryContext}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{definition.title.capitalize()}</h3>
      <RelatedObjectDefinitionDescription symbol={definition.symbol} parentTypeSymbol={definition.parentType.symbol} definingStatement={definition.definingStatement} />
      <hr/>
      <EditableSymbol symbol={definition.symbol} url={url} />
      <EditableExplicitName name={definition.explicitName} url={url} />
      <StatementDefinitionUsages usages={usages} statementDefinition={definition.statementDefinition} />
    </Page>
  </EntryContext.Provider>;
}
