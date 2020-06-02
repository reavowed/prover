import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";

export default function StandalonePropertyDescription({standalonePropertyDefinition}) {
  const entryContext = useContext(EntryContext);
  const variableDefinitions = [standalonePropertyDefinition.mainVariableDefinition];
  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(standalonePropertyDefinition.definingStatement, variableDefinitions, entryContext)}>
    {standalonePropertyDefinition.mainVariableDefinition.name} is {standalonePropertyDefinition.name} if <CopiableExpression expression={standalonePropertyDefinition.definingStatement} splitConjunction />.
  </DisplayContext.Provider>;
}
