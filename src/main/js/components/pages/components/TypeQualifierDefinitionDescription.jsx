import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import ChapterEntryWrapper from "../chapter/ChapterEntryWrapper";

export default function TypeQualifierDefinitionDescription({typeQualifierDefinition}) {
  const entryContext = useContext(EntryContext);
  const typeDefinition = typeQualifierDefinition.parentType;
  const termNames = [typeQualifierDefinition.parentType.defaultTermName, ...typeQualifierDefinition.qualifier.defaultTermNames];

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeQualifierDefinition.definingStatement, termNames, entryContext)}>
    {typeDefinition.article.capitalize()} <u>{typeDefinition.name} {formatQualifier(typeQualifierDefinition.qualifier)}</u> is a {typeDefinition.name} {typeDefinition.defaultTermName} such that <CopiableExpression expression={typeQualifierDefinition.definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
