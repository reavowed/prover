import * as path from "path";
import React from "react";
import EntryContext from "./EntryContext";
import {CopiableExpression} from "./ExpressionComponent";
import {Breadcrumbs} from "./pages/components/Breadcrumbs";
import EditableProperty from "./pages/components/EditableProperty";
import {NavLinks} from "./pages/components/NavLinks";
import {Usages} from "./pages/components/Usages";
import {Page} from "./pages/Page";

export function ExpressionDefinition({url, title, definition, bookLink, chapterLink, previous, next, usages, hasDisambiguator, children, entryContext}) {

  const saveSymbol = (newSymbol) => {
    return window.fetchJson(path.join(url, "symbol"), {method: "PUT", body: newSymbol})
      .then(url => window.location.pathname = url);
  };
  const saveDisambiguator = (newDisambiguator) => {
    return window.fetchJson(path.join(url, "disambiguator"), {method: "PUT", body: newDisambiguator})
      .then(url => window.location.pathname = url);
  };
  const saveName = (newName) => {
    return window.fetchJson(path.join(url, "name"), {method: "PUT", body: newName})
      .then(url => window.location.pathname = url);
  };
  const saveShorthand = (newName) => {
    return window.fetchJson(path.join(url, "shorthand"), {method: "PUT", body: newName});
  };
  const saveComponents = (componentsText) => {
    return window.fetchJson(path.join(url, "components"), {method: "PUT", body: componentsText})
  };
  const saveFormat = (newFormat) => {
    return window.fetchJson(path.join(url, "format"), {method: "PUT", body: newFormat});
  };
  const saveAttributes = (attributeText) => {
    return window.fetchJson(path.join(url, "attributes"), {method: "PUT", body: attributeText.split(" ")})
  };
  const saveDisambiguatorAdders = (newDisambiguatorAdders) => {
    return window.fetchJson(path.join(url, "disambiguatorAdders"), {method: "PUT", body: _.filter(newDisambiguatorAdders.split(/\r?\n/).map(_.trim), s => s.length)});
  };

  const serializedFormat = definition.format.originalValue ?
    "(" + definition.format.originalValue + ")" + (definition.format.requiresBrackets ? " requires-brackets" : "") + (definition.format.requiresComponentBrackets ? "" : " no-component-brackets") :
    "";

  return <EntryContext.Provider value={entryContext}>
    <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
      <NavLinks previous={previous} next={next} />
      <h3>{title}:  <CopiableExpression expression={definition.defaultValue} /></h3>
      {children}
      <hr/>
      <EditableProperty label="Symbol" initialValue={definition.baseSymbol} onSave={saveSymbol} />
      {hasDisambiguator && <EditableProperty label="Disambiguator" initialValue={definition.disambiguator} onSave={saveDisambiguator} />}
      <EditableProperty label="Name" initialValue={definition.explicitName} onSave={saveName} />
      <EditableProperty label="Shorthand" initialValue={definition.shorthand} onSave={saveShorthand} />
      <EditableProperty label="Components" initialValue={definition.componentTypes.map(x => x.name).join(" ")} onSave={saveComponents} />
      <EditableProperty label="Format" initialValue={serializedFormat} onSave={saveFormat} />
      <EditableProperty label="Attributes" initialValue={definition.attributes.join(" ")} onSave={saveAttributes} />
      {hasDisambiguator && <EditableProperty label="Disambiguator Adders" as="textarea" initialValue={definition.disambiguatorAdders.map(d => d.template.serializeNicely([["_"]]) + " " + d.disambiguator).join("\n")} onSave={saveDisambiguatorAdders} />}
      {definition.definitionInference && <Usages.ForInference usages={usages} inferenceId={definition.definitionInference.id} />}
      {definition.constructionInference && <Usages.ForInference usages={usages} inferenceId={definition.constructionInference.id} title="Construction" />}
      {definition.deconstructionInference && <Usages.ForInference usages={usages} inferenceId={definition.deconstructionInference.id} title="Deconstruction" />}
    </Page>
  </EntryContext.Provider>;
}
