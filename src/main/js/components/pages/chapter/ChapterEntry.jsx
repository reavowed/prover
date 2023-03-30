import React, {useContext} from "react";
import {DefinedExpression} from "../../../models/Expression";
import AvailableEntries from "../../AvailableEntries";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatHtml} from "../../helpers/Formatter";
import {ResultWithPremises} from "../../ResultWithPremises";
import PropertyOnTypeDefinitionDescription from "../components/PropertyOnTypeDefinitionDescription";
import RelatedObjectDefinitionDescription from "../components/RelatedObjectDefinitionDescription";
import StandalonePropertyDescription from "../components/StandalonePropertyDescription";
import TypeDefinitionDescription from "../components/TypeDefinitionDescription";
import TypeQualifierDefinitionDescription from "../components/TypeQualifierDefinitionDescription";
import TypeRelationDefinitionDescription from "../components/TypeRelationDefinitionDescription";
import ChapterContext from "./ChapterContext";
import ChapterEntryWrapper from "./ChapterEntryWrapper";
import DefinitionEntry from "./DefinitionEntry";
import DeleteEntryButton from "./DeleteEntryButton";
import InferenceEntry from "./InferenceEntry";

export default function ChapterEntry({entry, url, type, title}) {
  const availableEntries = useContext(AvailableEntries);
  switch (type) {
    case "axiom":
      return <InferenceEntry type="Axiom" inference={entry} url={url} />;
    case "theorem":
      return <InferenceEntry type="Theorem" inference={entry} url={url} incomplete={!entry.isComplete} />;
    case "statementDefinition":
      return <DefinitionEntry definition={entry} url={url}>
        {entry.definingStatement && <><CopiableExpression expression={entry.defaultValue}/> is defined as <CopiableExpression expression={entry.definingStatement}/>.</>}
      </DefinitionEntry>;
    case "termDefinition":
      const equality = _.find(availableEntries.definitions, d => _.includes(d.attributes, "equality"));
      const result = (equality && entry.definingStatement instanceof DefinedExpression && entry.definingStatement.definition === equality && entry.definingStatement.components[0].serialize() === entry.defaultValue.serialize()) ?
        <><CopiableExpression expression={entry.defaultValue}/> is defined to be equal to <CopiableExpression expression={entry.definingStatement.components[1]}/></> :
        <><CopiableExpression expression={entry.defaultValue}/> is defined such that <CopiableExpression expression={entry.definingStatement} splitConjunction /></>;
      return <DefinitionEntry definition={entry} url={url}>
        <ResultWithPremises premises={entry.premises}
                            result={result}/>
      </DefinitionEntry>;
    case "typeDefinition":
      return <ChapterEntryWrapper title={title} url={url}>
        <TypeDefinitionDescription typeDefinition={entry} />
      </ChapterEntryWrapper>;
    case "typeQualifierDefinition":
      return <ChapterEntryWrapper title={title} url={url}>
        <TypeQualifierDefinitionDescription typeQualifierDefinition={entry} />
      </ChapterEntryWrapper>;
    case "propertyDefinition": {
      return <ChapterEntryWrapper title={title} url={url}>
        <PropertyOnTypeDefinitionDescription propertyDefinition={entry} />
      </ChapterEntryWrapper>;
    }
    case "relatedObjectDefinition": {
      return <ChapterEntryWrapper title={title} url={url}>
        <RelatedObjectDefinitionDescription relatedObjectDefinition={entry} />
      </ChapterEntryWrapper>;
    }
    case "typeRelationDefinition": {
      return <ChapterEntryWrapper title={title} url={url}>
        <TypeRelationDefinitionDescription typeRelationDefinition={entry} />
      </ChapterEntryWrapper>;
    }
    case "standalonePropertyDefinition":
      return <ChapterEntryWrapper title={title} url={url}>
        <StandalonePropertyDescription standalonePropertyDefinition={entry} />
      </ChapterEntryWrapper>;
    case "comment":
      const chapterContext = useContext(ChapterContext);
      return <p>
        {chapterContext.editing && <span className="float-right" style={{marginRight: "11px"}}><DeleteEntryButton url={url} /></span>}
        {formatHtml(entry)}
      </p>;
    case "placeholder":
      return <React.Fragment />;
    default:
      throw `Unrecognised entry '${entry.type}'`;
  }
};
