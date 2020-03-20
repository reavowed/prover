import * as path from "path";
import React from "react";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {formatHtml, replacePlaceholders} from "../helpers/Formatter";
import {Breadcrumbs} from "./components/Breadcrumbs";
import EditableProperty from "./components/EditableProperty";
import {NavLinks} from "./components/NavLinks";
import {Usages} from "./components/Usages";
import {Page} from "./Page";

export function TypeDefinition({definition: definitionJson, definitions, typeDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, bookLink, chapterLink, url, previous, next, usages}) {
  const parser = new Parser(definitions, typeDefinitions);
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
  const saveComponents = (componentsText) => {
    return window.fetchJson(path.join(url, "components"), {method: "PUT", body: componentsText})
  };
  const saveFormat = (newFormat) => {
    return window.fetchJson(path.join(url, "format"), {method: "PUT", body: newFormat});
  };


  const serializedFormat = definition.componentFormat.originalValue ?
    "(" + definition.componentFormat.originalValue + ")" + (definition.componentFormat.requiresBrackets ? " requires-brackets" : "") + (definition.componentFormat.requiresComponentBrackets ? "" : " no-component-brackets") :
    "";

  return <EntryContext.Provider value={entryContext}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{definition.title.capitalize()}</h3>
      {definition.defaultTermName} is {definition.article} {definition.name} {formatHtml(definition.componentFormat.baseFormatString, s => replacePlaceholders(s, definition.otherComponentTypes.map(x => x.name)))} if <CopiableExpression expression={definition.definingStatement}/>.
      <hr/>
      <EditableProperty label="Symbol" initialValue={definition.symbol} onSave={saveSymbol} />
      <EditableProperty label="Name" initialValue={definition.explicitName} onSave={saveName} />
      <EditableProperty label="Components" initialValue={definition.otherComponentTypes.map(x => x.name).join(" ")} onSave={saveComponents} />
      <EditableProperty label="Component Format" initialValue={serializedFormat} onSave={saveFormat} />
      {usages.length > 0 && <><hr/><Usages usages={usages}/></>}
    </Page>
  </EntryContext.Provider>;
}
