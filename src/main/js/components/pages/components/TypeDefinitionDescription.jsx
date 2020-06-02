import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function TypeDefinitionDescription({typeDefinition}) {
  const entryContext = useContext(EntryContext);
  const termNames = [typeDefinition.defaultTermName, ...(typeDefinition.defaultQualifier ? typeDefinition.defaultQualifier.termNames : [])];
  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeDefinition.definingStatement, termNames, entryContext)}>
    {typeDefinition.defaultTermName} is {typeDefinition.article} <u>{typeDefinition.name}</u> {formatQualifier(typeDefinition.defaultQualifier)} if <CopiableExpression expression={typeDefinition.definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
