import React, {useContext} from "react";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "../../DisplaySettings";
import {CopiableExpression} from "../../expressions/ExpressionComponent";

export default function StandalonePropertyDescription({standalonePropertyDefinition}) {
  const availableEntries = useContext(AvailableEntriesContext);
  const variableDefinitions = [standalonePropertyDefinition.mainVariableDefinition];
  return <DisplaySettingsContext.Provider value={DisplaySettings.forTypeLikeDefinition(standalonePropertyDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {standalonePropertyDefinition.mainVariableDefinition.name} is {standalonePropertyDefinition.name} if <CopiableExpression expression={standalonePropertyDefinition.definingStatement} splitConjunction />.
  </DisplaySettingsContext.Provider>;
}
