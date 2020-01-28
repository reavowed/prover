import * as path from "path";
import React from "react";
import {Parser} from "../Parser";
import EntryContext from "./EntryContext";
import {CopiableExpression} from "./ExpressionComponent";
import {Breadcrumbs} from "./pages/components/Breadcrumbs";
import EditableProperty from "./pages/components/EditableProperty";
import {NavLinks} from "./pages/components/NavLinks";
import {Usages} from "./pages/components/Usages";
import {Page} from "./pages/Page";

export function ExpressionDefinition({url, title, definition, bookLink, chapterLink, previous, next, usages, children, entryContext}) {

  const saveSymbol = (newSymbol) => {
    return window.fetchJson(path.join(url, "symbol"), {method: "PUT", body: newSymbol});
  };
  const saveFormat = (newFormat) => {
    return window.fetchJson(path.join(url, "format"), {method: "PUT", body: newFormat});
  };
  const saveAttributes = (attributeText) => {
    return window.fetchJson(path.join(url, "attributes"), {method: "PUT", body: attributeText.split(" ")})
  };
  const serializedFormat = "(" + replacePlaceholders(definition.format.baseFormatString, [...definition.boundVariableNames, ...definition.componentTypes.map(c => c.name)]) + ")" + (definition.format.requiresBrackets ? " requires-brackets" : "") + (definition.format.requiresComponentBrackets ? "" : " no-component-brackets");

  function replacePlaceholders(text, replacements) {
    const matches = text.matchAll(/%(\d+)/g);
    let indexOfLastMatchEnd = 0;
    let result = "";
    for (const match of matches) {
      result += text.substr(indexOfLastMatchEnd, match.index - indexOfLastMatchEnd);
      result += replacements[parseInt(match[1])];
      indexOfLastMatchEnd = match.index + match[0].length;
    }
    result += text.substr(indexOfLastMatchEnd);
    return result;
  }

  return <EntryContext.Provider value={entryContext}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title, url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{title}:  <CopiableExpression expression={definition.defaultValue} /></h3>
      {children}
      <hr/>
      <EditableProperty label="Symbol" initialValue={definition.symbol} onSave={saveSymbol} />
      <EditableProperty label="Format" initialValue={serializedFormat} onSave={saveFormat} />
      <EditableProperty label="Attributes" initialValue={definition.attributes.join(" ")} onSave={saveAttributes} />

      {usages.length > 0 && <><hr/><Usages usages={usages}/></>}
    </Page>
  </EntryContext.Provider>;
}