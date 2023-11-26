import React, {Fragment, useContext} from "react";
import DisplayContext from "../../DisplayContext";
import AvailableEntries from "../../AvailableEntries";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function TypeDefinitionDescription({typeDefinition}) {
  const availableEntries = useContext(AvailableEntries);
  let description = typeDefinition.name;
  let qualifierDescription = formatQualifier(typeDefinition.defaultQualifier);
  if (qualifierDescription) {
    description = <Fragment>{description} {qualifierDescription}</Fragment>;
  }
  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeDefinition.definingStatement, typeDefinition.allVariableDefinitions, availableEntries)}>
    {typeDefinition.mainVariableDefinition.name} is {typeDefinition.article} <u>{description}</u> if <CopiableExpression expression={typeDefinition.definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
