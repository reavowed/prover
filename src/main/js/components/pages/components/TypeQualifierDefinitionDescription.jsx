import React, {useContext} from "react";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "../../DisplaySettings";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function TypeQualifierDefinitionDescription({typeQualifierDefinition}) {
  const availableEntries = useContext(AvailableEntriesContext);
  const typeDefinition = typeQualifierDefinition.parentType;
  const variableDefinitions = [typeQualifierDefinition.parentType.mainVariableDefinition, ...typeQualifierDefinition.qualifier.variableDefinitions];

  return <DisplaySettingsContext.Provider value={DisplaySettings.forTypeLikeDefinition(typeQualifierDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {typeDefinition.article.capitalize()} <u>{typeDefinition.name} {formatQualifier(typeQualifierDefinition.qualifier)}</u> is a {typeDefinition.name} {typeDefinition.mainVariableDefinition.name} such that <CopiableExpression expression={typeQualifierDefinition.definingStatement} splitConjunction/>.
  </DisplaySettingsContext.Provider>;
}
