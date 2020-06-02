import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";

export default function StandalonePropertyDescription({standalonePropertyDefinition}) {
  const entryContext = useContext(EntryContext);
  const termNames = [standalonePropertyDefinition.mainVariableDefinition.name];
  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(standalonePropertyDefinition.definingStatement, termNames, entryContext)}>
    {standalonePropertyDefinition.mainVariableDefinition.name} is {standalonePropertyDefinition.name} if <CopiableExpression expression={standalonePropertyDefinition.definingStatement} splitConjunction />.
  </DisplayContext.Provider>;
}
