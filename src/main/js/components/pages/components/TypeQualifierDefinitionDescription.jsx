import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import ChapterEntryWrapper from "../chapter/ChapterEntryWrapper";

export default function TypeQualifierDefinitionDescription({typeQualifierDefinition}) {
  const availableEntries = useContext(AvailableEntriesContext);
  const typeDefinition = typeQualifierDefinition.parentType;
  const variableDefinitions = [typeQualifierDefinition.parentType.mainVariableDefinition, ...typeQualifierDefinition.qualifier.variableDefinitions];

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeQualifierDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {typeDefinition.article.capitalize()} <u>{typeDefinition.name} {formatQualifier(typeQualifierDefinition.qualifier)}</u> is a {typeDefinition.name} {typeDefinition.mainVariableDefinition.name} such that <CopiableExpression expression={typeQualifierDefinition.definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
