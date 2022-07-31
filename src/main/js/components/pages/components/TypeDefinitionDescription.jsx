import React, {Fragment, useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function TypeDefinitionDescription({typeDefinition}) {
  const entryContext = useContext(EntryContext);
  let description = typeDefinition.name;
  let qualifierDescription = formatQualifier(typeDefinition.defaultQualifier);
  if (qualifierDescription) {
    description = <Fragment>{description} {qualifierDescription}</Fragment>;
  }
  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeDefinition.definingStatement, typeDefinition.allVariableDefinitions, entryContext)}>
    {typeDefinition.mainVariableDefinition.name} is {typeDefinition.article} <u>{description}</u> if <CopiableExpression expression={typeDefinition.definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
