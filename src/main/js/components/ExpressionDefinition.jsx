import React from "react";
import AvailableEntriesContext from "./AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "./DisplaySettings";
import {CopiableExpression} from "./expressions/ExpressionComponent";
import InputWithShorthandReplacement from "./helpers/InputWithShorthandReplacement";
import {Breadcrumbs} from "./pages/components/Breadcrumbs";
import EditableProperties from "./pages/components/EditableProperties";
import {NavLinks} from "./pages/components/NavLinks";
import Usages from "./pages/components/Usages";
import {Page} from "./pages/Page";

export function ExpressionDefinition({url, title, definition, setDefinition, bookLink, chapterLink, previous, next, usages, hasDisambiguator, children, availableEntries}) {
  const displaySettings = DisplaySettings.forExpressionDefinition(definition, availableEntries);
  const serializedFormat = definition.format.originalValue ?
    "(" + definition.format.originalValue + ")" + (definition.format.requiresBrackets ? " requires-brackets" : "") + (definition.format.requiresComponentBrackets ? "" : " no-component-brackets") :
    "";

  const editableProperties = [
    {label: "Symbol", initialValue: definition.baseSymbol, endpointName: "symbol"},
    hasDisambiguator && { label: "Disambiguator", initialValue: definition.disambiguator, endpointName: "disambiguator"},
    {label: "Explicit Name", initialValue: definition.explicitName, endpointName: "name"},
    {label: "Shorthand", initialValue: definition.shorthand, endpointName: "shorthand"},
    {label: "Components", initialValue: definition.componentTypes.map(x => x.name).join(" ")},
    {label: "Format", initialValue: serializedFormat, endpointName: "format"},
    {label: "Attributes", initialValue: definition.attributes.join(" "), endpointName: "attributes", process: newAttributesText => newAttributesText.split(" ")},
    hasDisambiguator && {
      label: "Disambiguator Adders",
      initialValue: definition.disambiguatorAdders.map(d => d.template.serializeNicely([["_"]], displaySettings.variableDefinitions) + " " + d.disambiguator).join("\n"),
      endpointName: "disambiguator",
      process: newDisambiguatorAddersText => _.filter(newDisambiguatorAddersText.split(/\r?\n/).map(_.trim), s => s.length),
      inputType: InputWithShorthandReplacement,
      inputProps: {as: "textarea"}
    },
    definition.definitionPredicate && {
      label: "Definition Predicate",
      initialValue: definition.definitionPredicate.serializeNicely([["_"]], displaySettings.variableDefinitions),
      endpointName: "definingStatement"
    }
  ];

  return <AvailableEntriesContext.Provider value={availableEntries}>
    <DisplaySettingsContext.Provider value={displaySettings}>
      <Page breadcrumbs={<Breadcrumbs links={[bookLink, chapterLink, {title: definition.title.capitalize(), url}]}/>}>
        <NavLinks previous={previous} next={next} />
        <h3>{title}:  <CopiableExpression expression={definition.defaultValue} /></h3>
        {children}
        <EditableProperties url={url} updateEntry={setDefinition} definitions={editableProperties} />
        {definition.definitionInference && <Usages.ForInference usages={usages} inferenceId={definition.definitionInference.id} />}
        {definition.constructionInference && <Usages.ForInference usages={usages} inferenceId={definition.constructionInference.id} title="Construction" />}
        {definition.deconstructionInference && <Usages.ForInference usages={usages} inferenceId={definition.deconstructionInference.id} title="Deconstruction" />}
      </Page>
    </DisplaySettingsContext.Provider>
  </AvailableEntriesContext.Provider>;
}
