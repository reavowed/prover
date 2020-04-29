import * as path from "path";
import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {formatHtml, replacePlaceholders} from "../helpers/Formatter";
import ChapterEntryWrapper from "./chapter/ChapterEntryWrapper";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableExplicitName from "./components/EditableExplicitName";
import EditableProperty from "./components/EditableProperty";
import EditableSymbol from "./components/EditableSymbol";
import {NavLinks} from "./components/NavLinks";
import {Usages} from "./components/Usages";
import {Page} from "./Page";

export function RelatedObjectDefinition({definition: definitionJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, bookLink, chapterLink, url, previous, next, usages}) {
  const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
  const definition = parser.parsePropertyDefinition(definitionJson);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);

  const qualifier = definition.requiredParentQualifier?.qualifier || definition.parentType.defaultQualifier;

  return <DisplayContext.Provider value={DisplayContext.forTypeDefinition(definition, entryContext)}>
    <EntryContext.Provider value={entryContext}>
      <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
        <NavLinks previous={previous} next={next} />
        <h3>{definition.title.capitalize()}</h3>
        {definition.article.capitalize()} {definition.name} for {definition.parentType.article} {definition.parentType.name} {definition.parentType.defaultTermName} {qualifier && formatHtml(qualifier.format, s => replacePlaceholders(s, qualifier.defaultTermNames))} is an object {definition.defaultTermName} such that <CopiableExpression expression={definition.definingStatement} splitConjunction/>.
        <hr/>
        <EditableSymbol symbol={definition.symbol} url={url} />
        <EditableExplicitName name={definition.explicitName} url={url} />
        <Usages.ForInference usages={usages} inferenceId={definition.statementDefinition.constructionInference.id} title="Construction" />
        <Usages.ForInference usages={usages} inferenceId={definition.statementDefinition.deconstructionInference.id} title="Deconstruction" />
      </Page>
    </EntryContext.Provider>
  </DisplayContext.Provider>;
}
