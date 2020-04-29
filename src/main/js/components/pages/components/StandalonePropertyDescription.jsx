import React, {useContext} from "react";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import {Page} from "../Page";

export default function StandalonePropertyDescription({symbol, definingStatement}) {
  const entryContext = useContext(EntryContext);
  const definition = entryContext.standalonePropertyDefinitions[symbol];
  return <>
    {definition.defaultTermName} is {definition.name} if <CopiableExpression expression={definingStatement} splitConjunction />.
  </>;
}
