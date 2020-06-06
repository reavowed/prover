import React from "react";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {Breadcrumbs} from "./components/Breadcrumbs";
import {NavLinks} from "./components/NavLinks";
import StatementDefinitionUsages from "./components/StatementDefinitionUsages";
import TypeQualifierDefinitionDescription from "./components/TypeQualifierDefinitionDescription";
import {Page} from "./Page";
import {useMappedState} from "./utils/entryFunctions";

export function TypeQualifierDefinition(props) {
  const {definition: definitionJson, bookLink, chapterLink, url, previous, next, usages} = props;
  const [parser, entryContext] = EntryContext.fromEntryProps(props);
  const [definition, setDefinition] = useMappedState(definitionJson, parser.parseDefinitionWithDefiningStatement);

  return <EntryContext.Provider value={entryContext}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{definition.title.capitalize()}</h3>
      <TypeQualifierDefinitionDescription typeQualifierDefinition={definition} />
      <StatementDefinitionUsages usages={usages} statementDefinition={definition.statementDefinition} />
    </Page>
  </EntryContext.Provider>;
}
