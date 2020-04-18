import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {formatHtml, replacePlaceholders} from "../helpers/Formatter";
import {Breadcrumbs} from "./components/Breadcrumbs";
import {NavLinks} from "./components/NavLinks";
import {Usages} from "./components/Usages";
import {Page} from "./Page";

export function PropertyDefinitionOnType({definition: definitionJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, bookLink, chapterLink, url, previous, next, usages}) {
  const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
  const definition = parser.parsePropertyDefinition(definitionJson);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);

  return <DisplayContext.Provider value={DisplayContext.forTypeDefinition(definition, entryContext)}>
    <EntryContext.Provider value={entryContext}>
      <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
        <NavLinks previous={previous} next={next} />
        <h3>{definition.title.capitalize()}</h3>
        {definition.parentType.article.capitalize()} {definition.parentType.name} {definition.defaultTermName} {definition.parentType.qualifier && formatHtml(definition.parentType.qualifier.format.baseFormatString, s => replacePlaceholders(s, definition.parentType.qualifier.termNames))} is {definition.name} if <CopiableExpression expression={definition.definingStatement}/>.
        <Usages.ForInference usages={usages} inferenceId={definition.statementDefinition.constructionInference.id} title="Construction" />
        <Usages.ForInference usages={usages} inferenceId={definition.statementDefinition.deconstructionInference.id} title="Deconstruction" />
      </Page>
    </EntryContext.Provider>
  </DisplayContext.Provider>;
}
