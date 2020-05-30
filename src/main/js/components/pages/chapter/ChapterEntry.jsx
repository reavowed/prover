import React, {useContext} from "react";
import {DefinedExpression} from "../../../models/Expression";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatHtml} from "../../helpers/Formatter";
import {ResultWithPremises} from "../../ResultWithPremises";
import PropertyOnTypeDefinitionDescription from "../components/PropertyOnTypeDefinitionDescription";
import RelatedObjectDefinitionDescription from "../components/RelatedObjectDefinitionDescription";
import StandalonePropertyDescription from "../components/StandalonePropertyDescription";
import TypeDefinitionDescription from "../components/TypeDefinitionDescription";
import TypeQualifierDefinitionDescription from "../components/TypeQualifierDefinitionDescription";
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
        <DefinitionEntry key={entry.url} entry={entry}>
          {entry.definingStatement && <><CopiableExpression expression={entry.defaultValue}/> is defined as <CopiableExpression expression={entry.definingStatement}/>.</>}
        </DefinitionEntry>
      </DisplayContext.Provider>;
    case "termDefinition":
      const equality = _.find(entryContext.definitions, d => _.includes(d.attributes, "equality"));
      const result = (equality && entry.definingStatement instanceof DefinedExpression && entry.definingStatement.definition === equality && entry.definingStatement.components[0].serialize() === entry.defaultValue.serialize()) ?
        <><CopiableExpression expression={entry.defaultValue}/> is defined to be equal to <CopiableExpression expression={entry.definingStatement.components[1]}/></> :
        <><CopiableExpression expression={entry.defaultValue}/> is defined such that <CopiableExpression expression={entry.definingStatement} splitConjunction /></>;
      return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(entry, entryContext)}>
        <DefinitionEntry key={entry.url} entry={entry}>
          <ResultWithPremises premises={entry.premises}
                              result={result}/>
        </DefinitionEntry>
      </DisplayContext.Provider>;
    case "typeDefinition":
      return <ChapterEntryWrapper title={entry.title} url={entry.url} key={entry.url}>
        <TypeDefinitionDescription symbol={entry.symbol} definingStatement={entry.definingStatement} />
      </ChapterEntryWrapper>;
    case "typeQualifierDefinition":
      return <ChapterEntryWrapper title={entry.title} url={entry.url} key={entry.url}>
        <TypeQualifierDefinitionDescription symbol={entry.symbol} parentTypeSymbol={entry.parentTypeSymbol} definingStatement={entry.definingStatement} />
      </ChapterEntryWrapper>;
    case "propertyDefinition": {
      return <ChapterEntryWrapper title={entry.title} url={entry.url} key={entry.url}>
        <PropertyOnTypeDefinitionDescription symbol={entry.symbol} parentTypeSymbol={entry.parentTypeSymbol} definingStatement={entry.definingStatement} />
      </ChapterEntryWrapper>;
    }
    case "relatedObjectDefinition": {
      return <ChapterEntryWrapper title={entry.title} url={entry.url} key={entry.url}>
        <RelatedObjectDefinitionDescription symbol={entry.symbol} parentTypeSymbol={entry.parentTypeSymbol} definingStatement={entry.definingStatement} />
      </ChapterEntryWrapper>;
    }
    case "standalonePropertyDefinition":
      return <ChapterEntryWrapper title={entry.title} url={entry.url} key={entry.url}>
        <StandalonePropertyDescription symbol={entry.symbol} definingStatement={entry.definingStatement} />
      </ChapterEntryWrapper>;
    case "comment":
      const chapterContext = useContext(ChapterContext);
      return <p key={entry.url}>
        {chapterContext.editing && <span className="float-right" style={{marginRight: "11px"}}><DeleteEntryButton url={entry.url} /></span>}
        {formatHtml(entry.text)}
      </p>;
    case "placeholder":
      return <React.Fragment key={entry.url}/>;
    default:
      throw `Unrecognised entry '${entry.type}'`;
  }
};
