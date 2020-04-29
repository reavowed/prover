import React, {useContext} from "react";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatHtml, formatQualifier, replacePlaceholders} from "../../helpers/Formatter";
import ChapterEntryWrapper from "../chapter/ChapterEntryWrapper";

export default function RelatedObjectDefinitionDescription({symbol, parentTypeSymbol, definingStatement}) {
  const entryContext = useContext(EntryContext);
  const typeDefinition = entryContext.typeDefinitions[parentTypeSymbol];
  const relatedObjectDefinition = _.find(typeDefinition.relatedObjects, p => p.symbol === symbol);
  const qualifier = relatedObjectDefinition.requiredParentQualifier ?
    _.find(typeDefinition.qualifiers, q => q.symbol === relatedObjectDefinition.requiredParentQualifier).qualifier :
    typeDefinition.defaultQualifier;

  return <>
    {relatedObjectDefinition.article.capitalize()} {relatedObjectDefinition.name} for {typeDefinition.article} {typeDefinition.name} {typeDefinition.defaultTermName} {formatQualifier(qualifier)} is an object {relatedObjectDefinition.defaultTermName} such that <CopiableExpression expression={definingStatement} splitConjunction/>.
  </>;
}
