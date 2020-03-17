import React, {useContext} from "react";
import styled from "styled-components";
import {DefinedExpression} from "../../../models/Expression";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatHtml, replacePlaceholders} from "../../helpers/Formatter";
import {ResultWithPremises} from "../../ResultWithPremises";
import ChapterContext from "./ChapterContext";
import DefinitionEntry from "./DefinitionEntry";
import DeleteEntryButton from "./DeleteEntryButton";
import InferenceEntry from "./InferenceEntry";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

const Capitalized = styled.span`
  text-transform: capitalize;
`;

export default function ChapterEntry({entry}) {
  const entryContext = useContext(EntryContext);
  switch (entry.type) {
    case "axiom":
      return <InferenceEntry key={entry.url} title="Axiom" entry={entry} />;
    case "theorem":
      return <InferenceEntry key={entry.url} title="Theorem" entry={entry} incomplete={!entry.isComplete} />;
    case "statementDefinition":
      return <DefinitionEntry key={entry.url} title="Statement Definition" entry={entry}>
        {entry.definingStatement && <><CopiableExpression expression={entry.defaultValue}/> is defined as <CopiableExpression expression={entry.definingStatement}/>.</>}
      </DefinitionEntry>;
    case "termDefinition":
      const equality = _.find(entryContext.definitions, d => _.includes(d.attributes, "equality"));
      const result = (equality && entry.definingStatement instanceof DefinedExpression && entry.definingStatement.definition === equality && entry.definingStatement.components[0].serialize() === entry.defaultValue.serialize()) ?
        <><CopiableExpression expression={entry.defaultValue}/> is defined to be equal to <CopiableExpression expression={entry.definingStatement.components[1]}/></> :
        <><CopiableExpression expression={entry.defaultValue}/> is defined by <CopiableExpression expression={entry.definingStatement}/></>;
      return <DefinitionEntry key={entry.url} title="Term Definition" entry={entry}>
        <ResultWithPremises premises={entry.premises}
                            result={result}/>
      </DefinitionEntry>;
    case "typeDefinition":
      const definition = entryContext.typeDefinitions[entry.symbol];
      return <ChapterEntryWrapper title={<>Definition: <Capitalized>{definition.name}</Capitalized></>}
                                  url={entry.url}
                                  key={entry.url}>
        {entry.defaultTermName} is {definition.article} {definition.name} {formatHtml(definition.componentFormatString, s => replacePlaceholders(s, entry.components))} if <CopiableExpression expression={entry.definingStatement}/>.
      </ChapterEntryWrapper>;
    case "propertyDefinition":
      const typeDefinition = entryContext.typeDefinitions[entry.parentTypeSymbol];
      return <ChapterEntryWrapper title={<>Definition: <span style={{textTransform: "capitalize"}}>{entry.name} {typeDefinition.name}</span></>}
                                  url={entry.url}
                                  key={entry.url}>
        <Capitalized>{typeDefinition.article}</Capitalized> {typeDefinition.name} {entry.defaultTermName} {formatHtml(typeDefinition.componentFormatString, s => replacePlaceholders(s, entry.parentTypeComponents))} is {entry.name} if <CopiableExpression expression={entry.definingStatement}/>.
      </ChapterEntryWrapper>;
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
