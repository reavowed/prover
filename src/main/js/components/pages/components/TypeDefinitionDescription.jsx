import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function TypeDefinitionDescription({symbol, definingStatement}) {
  const entryContext = useContext(EntryContext);
  const definition = entryContext.typeDefinitions[symbol];
  const termNames = [definition.defaultTermName, ...(definition.defaultQualifier ? definition.defaultQualifier.termNames : [])];
  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(definingStatement, termNames, entryContext)}>
    {definition.defaultTermName} is {definition.article} <u>{definition.name}</u> {formatQualifier(definition.defaultQualifier)} if <CopiableExpression expression={definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
