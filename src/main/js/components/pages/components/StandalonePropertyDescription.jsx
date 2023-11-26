import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import {CopiableExpression} from "../../expressions/ExpressionComponent";

export default function StandalonePropertyDescription({standalonePropertyDefinition}) {
  const availableEntries = useContext(AvailableEntriesContext);
  const variableDefinitions = [standalonePropertyDefinition.mainVariableDefinition];
  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(standalonePropertyDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {standalonePropertyDefinition.mainVariableDefinition.name} is {standalonePropertyDefinition.name} if <CopiableExpression expression={standalonePropertyDefinition.definingStatement} splitConjunction />.
  </DisplayContext.Provider>;
}
