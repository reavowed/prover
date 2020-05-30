import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import {Page} from "../Page";

export default function StandalonePropertyDescription({symbol, definingStatement}) {
  const entryContext = useContext(EntryContext);
  const definition = entryContext.standalonePropertyDefinitions[symbol];
  const termNames = [definition.defaultTermName];
  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(definingStatement, termNames, entryContext)}>
    {definition.defaultTermName} is {definition.name} if <CopiableExpression expression={definingStatement} splitConjunction />.
  </DisplayContext.Provider>;
}
