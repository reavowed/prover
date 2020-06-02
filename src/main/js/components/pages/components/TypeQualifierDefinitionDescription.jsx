import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import ChapterEntryWrapper from "../chapter/ChapterEntryWrapper";

export default function TypeQualifierDefinitionDescription({typeQualifierDefinition}) {
  const entryContext = useContext(EntryContext);
  const typeDefinition = typeQualifierDefinition.parentType;
  const termNames = [typeQualifierDefinition.parentType.mainVariableDefinition.name, ...typeQualifierDefinition.qualifier.variableDefinitions.map(d => d.name)];

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeQualifierDefinition.definingStatement, termNames, entryContext)}>
    {typeDefinition.article.capitalize()} <u>{typeDefinition.name} {formatQualifier(typeQualifierDefinition.qualifier)}</u> is a {typeDefinition.name} {typeDefinition.mainVariableDefinition.name} such that <CopiableExpression expression={typeQualifierDefinition.definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
