import React, {useContext} from "react";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function TypeDefinitionDescription({symbol, definingStatement}) {
  const entryContext = useContext(EntryContext);
  const definition = entryContext.typeDefinitions[symbol];
  return <>
    {definition.defaultTermName} is {definition.article} {definition.name} {formatQualifier(definition.defaultQualifier)} if <CopiableExpression expression={definingStatement} splitConjunction/>.
  </>;
}
