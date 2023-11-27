import React, {Fragment, useContext} from "react";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "../../DisplaySettings";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function TypeDefinitionDescription({typeDefinition}) {
  const availableEntries = useContext(AvailableEntriesContext);
  let description = typeDefinition.name;
  let qualifierDescription = formatQualifier(typeDefinition.defaultQualifier);
  if (qualifierDescription) {
    description = <Fragment>{description} {qualifierDescription}</Fragment>;
  }
  return <DisplaySettingsContext.Provider value={DisplaySettings.forTypeLikeDefinition(typeDefinition.definingStatement, typeDefinition.allVariableDefinitions, availableEntries)}>
    {typeDefinition.mainVariableDefinition.name} is {typeDefinition.article} <u>{description}</u> if <CopiableExpression expression={typeDefinition.definingStatement} splitConjunction/>.
  </DisplaySettingsContext.Provider>;
}
