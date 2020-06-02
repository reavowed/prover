import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatHtml, formatQualifier, replacePlaceholders} from "../../helpers/Formatter";
import ChapterEntryWrapper from "../chapter/ChapterEntryWrapper";

export default function RelatedObjectDefinitionDescription({relatedObjectDefinition}) {
  const entryContext = useContext(EntryContext);
  const typeDefinition = relatedObjectDefinition.parentType;
  const qualifier = relatedObjectDefinition.requiredParentQualifier || typeDefinition.defaultQualifier;
  const termNames = [relatedObjectDefinition.defaultTermName, typeDefinition.defaultTermName, ...[qualifier ? qualifier.defaultTermNames : []]];

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(relatedObjectDefinition.definingStatement, termNames, entryContext)}>
    {relatedObjectDefinition.article.capitalize()} <u>{relatedObjectDefinition.name}</u> for {typeDefinition.article} {typeDefinition.name} {typeDefinition.defaultTermName} {formatQualifier(qualifier)} is an object {relatedObjectDefinition.defaultTermName} such that <CopiableExpression expression={relatedObjectDefinition.definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
