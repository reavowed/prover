import React from "react";
import {Parser} from "../../Parser";
import AvailableEntries from "../AvailableEntries";
import {Breadcrumbs} from "./components/Breadcrumbs";
import {NavLinks} from "./components/NavLinks";
import StandalonePropertyDescription from "./components/StandalonePropertyDescription";
import StatementDefinitionUsages from "./components/StatementDefinitionUsages";
import {Page} from "./Page";

export function StandalonePropertyDefinition(props) {
  const {definition: definitionJson, bookLink, chapterLink, url, previous, next, usages} = props;
  const [parser, availableEntries] = AvailableEntries.fromEntryProps(props);
  const definition = parser.parseDefinitionWithDefiningStatement(definitionJson);

  return <AvailableEntries.Provider value={availableEntries}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{definition.title.capitalize()}</h3>
      <StandalonePropertyDescription standalonePropertyDefinition={definition} />
      <StatementDefinitionUsages usages={usages} statementDefinition={definition.statementDefinition} />
    </Page>
  </AvailableEntries.Provider>;
}
