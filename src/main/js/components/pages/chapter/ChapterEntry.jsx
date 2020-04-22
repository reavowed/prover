import React, {useContext} from "react";
import {DefinedExpression} from "../../../models/Expression";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatHtml, replacePlaceholders} from "../../helpers/Formatter";
import {ResultWithPremises} from "../../ResultWithPremises";
import ChapterContext from "./ChapterContext";
import ChapterEntryWrapper from "./ChapterEntryWrapper";
import DefinitionEntry from "./DefinitionEntry";
import DeleteEntryButton from "./DeleteEntryButton";
import InferenceEntry from "./InferenceEntry";

export default function ChapterEntry({entry}) {
  const entryContext = useContext(EntryContext);
  switch (entry.type) {
    case "axiom":
      return <DisplayContext.Provider value={DisplayContext.forInferenceSummary(entry, entryContext)}>
        <InferenceEntry key={entry.url} title="Axiom" entry={entry} />
      </DisplayContext.Provider>;
    case "theorem":
      return <DisplayContext.Provider value={DisplayContext.forInferenceSummary(entry, entryContext)}>
          <InferenceEntry key={entry.url} title="Theorem" entry={entry} incomplete={!entry.isComplete} />
        </DisplayContext.Provider>;
    case "statementDefinition":
      return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(entry, entryContext)}>
        <DefinitionEntry key={entry.url} title="Statement Definition" entry={entry}>
          {entry.definingStatement && <><CopiableExpression expression={entry.defaultValue}/> is defined as <CopiableExpression expression={entry.definingStatement}/>.</>}
        </DefinitionEntry>
      </DisplayContext.Provider>;
    case "termDefinition":
      const equality = _.find(entryContext.definitions, d => _.includes(d.attributes, "equality"));
      const result = (equality && entry.definingStatement instanceof DefinedExpression && entry.definingStatement.definition === equality && entry.definingStatement.components[0].serialize() === entry.defaultValue.serialize()) ?
        <><CopiableExpression expression={entry.defaultValue}/> is defined to be equal to <CopiableExpression expression={entry.definingStatement.components[1]}/></> :
        <><CopiableExpression expression={entry.defaultValue}/> is defined such that <CopiableExpression expression={entry.definingStatement} splitConjunction /></>;
      return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(entry, entryContext)}>
        <DefinitionEntry key={entry.url} title="Term Definition" entry={entry}>
          <ResultWithPremises premises={entry.premises}
                              result={result}/>
        </DefinitionEntry>
      </DisplayContext.Provider>;
    case "typeDefinition":
      const definition = entryContext.typeDefinitions[entry.symbol];
      return <DisplayContext.Provider value={DisplayContext.forTypeDefinition(entry, entryContext)}>
        <ChapterEntryWrapper title={<>Definition: {definition.name.capitalize()}</>}
                             url={entry.url}
                             key={entry.url}>
          {entry.defaultTermName} is {definition.article} {definition.name} {definition.defaultQualifier && formatHtml(definition.defaultQualifier.format, s => replacePlaceholders(s, entry.defaultQualifierTermNames))} if <CopiableExpression expression={entry.definingStatement} splitConjunction/>.
        </ChapterEntryWrapper>
      </DisplayContext.Provider>;
    case "typeQualifierDefinition":
      const typeDefinition = entryContext.typeDefinitions[entry.parentTypeSymbol];
      const qualifierDefinition = _.find(typeDefinition.qualifiers, q => q.symbol === entry.symbol);
      return <DisplayContext.Provider value={DisplayContext.forTypeDefinition(entry, entryContext)}>
        <ChapterEntryWrapper
          title={<>Definition: {typeDefinition.name.capitalize()} {qualifierDefinition.name.capitalize()}</>}
          url={entry.url}
          key={entry.url}>
          {typeDefinition.article.capitalize()} {typeDefinition.name} {formatHtml(qualifierDefinition.qualifier.format, s => replacePlaceholders(s, entry.qualifierTermNames))} is a {typeDefinition.name} {entry.defaultTermName} such that <CopiableExpression expression={entry.definingStatement} splitConjunction/>.
        </ChapterEntryWrapper>
      </DisplayContext.Provider>;
    case "propertyDefinition": {
      const typeDefinition = entryContext.typeDefinitions[entry.parentTypeSymbol];
      const propertyDefinition = _.find(typeDefinition.properties, p => p.symbol === entry.symbol);
      const qualifier = propertyDefinition.requiredParentQualifier ?
        _.find(typeDefinition.qualifiers, q => q.symbol === propertyDefinition.requiredParentQualifier).qualifier :
        typeDefinition.defaultQualifier;

      return <DisplayContext.Provider value={DisplayContext.forTypeDefinition(entry, entryContext)}>
        <ChapterEntryWrapper
          title={<>Definition: {entry.name.capitalize()} {typeDefinition.name.capitalize()}</>}
          url={entry.url}
          key={entry.url}>
          {typeDefinition.article.capitalize()} {typeDefinition.name} {typeDefinition.defaultTermName} {qualifier && formatHtml(qualifier.format, s => replacePlaceholders(s, qualifier.defaultTermNames))} is {entry.name} if <CopiableExpression expression={entry.definingStatement} splitConjunction/>.
        </ChapterEntryWrapper>
      </DisplayContext.Provider>;
    }
    case "standalonePropertyDefinition":
      return <DisplayContext.Provider value={DisplayContext.forTypeDefinition(entry, entryContext)}>
        <ChapterEntryWrapper title={<>Definition: {entry.name.capitalize()}</>}
                             url={entry.url}
                             key={entry.url}>
          {entry.defaultTermName} is {entry.name} if <CopiableExpression expression={entry.definingStatement}/>.
        </ChapterEntryWrapper>
      </DisplayContext.Provider>;
    case "comment":
      const chapterContext = useContext(ChapterContext);
      return <p key={entry.url}>
        {chapterContext.editing && <span className="float-right" style={{marginRight: "11px"}}><DeleteEntryButton url={entry.url} /></span>}
        {entry.text}
      </p>;
    case "placeholder":
      return <React.Fragment key={entry.url}/>;
    default:
      throw `Unrecognised entry '${entry.type}'`;
  }
};
