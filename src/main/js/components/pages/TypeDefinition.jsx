import * as path from "path";
import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {formatHtml, replacePlaceholders} from "../helpers/Formatter";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableProperty from "./components/EditableProperty";
import {NavLinks} from "./components/NavLinks";
import {Usages} from "./components/Usages";
import {Page} from "./Page";

export function TypeDefinition({definition: definitionJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, bookLink, chapterLink, url, previous, next, usages}) {
  const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
  const definition = parser.parseTypeDefinition(definitionJson);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);

  const saveSymbol = (newSymbol) => {
    return window.fetchJson(path.join(url, "symbol"), {method: "PUT", body: newSymbol})
      .then(url => window.location.pathname = url);
  };
  const saveName = (newName) => {
    return window.fetchJson(path.join(url, "name"), {method: "PUT", body: newName})
      .then(url => window.location.pathname = url);
  };
  const saveFormat = (newFormat) => {
    return window.fetchJson(path.join(url, "format"), {method: "PUT", body: newFormat});
  };

  const serializedFormat = definition.qualifier.format.originalValue ?
    "(" + definition.qualifier.format.originalValue + ")" + (definition.qualifier.format.requiresBrackets ? " requires-brackets" : "") + (definition.qualifier.format.requiresComponentBrackets ? "" : " no-component-brackets") :
    "";

  return <DisplayContext.Provider value={DisplayContext.forTypeDefinition(definition, entryContext)}>
    <EntryContext.Provider value={entryContext}>
      <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
        <NavLinks previous={previous} next={next} />
        <h3>{definition.title.capitalize()}</h3>
        {definition.defaultTermName} is {definition.article} {definition.name} {definition.qualifier && formatHtml(definition.qualifier.format.baseFormatString, s => replacePlaceholders(s, definition.qualifier.termNames))} if <CopiableExpression expression={definition.definingStatement}/>.
        <hr/>
        <EditableProperty label="Symbol" initialValue={definition.symbol} onSave={saveSymbol} />
        <EditableProperty label="Explicit Name" initialValue={definition.explicitName} onSave={saveName} />
        <EditableProperty label="Main Term Name" initialValue={definition.defaultTermName} />
        <EditableProperty label="Qualifier Term Names" initialValue={definition.qualifier.termNames.join(" ")} />
        <EditableProperty label="Qualifier Format" initialValue={serializedFormat} onSave={saveFormat} />
        <Usages.ForInference usages={usages} inferenceId={definition.statementDefinition.constructionInference.id} title="Construction" />
        <Usages.ForInference usages={usages} inferenceId={definition.statementDefinition.deconstructionInference.id} title="Deconstruction" />
      </Page>
    </EntryContext.Provider>
  </DisplayContext.Provider>;
}
