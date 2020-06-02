import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import {joinAsList} from "../../helpers/reactFunctions";
import BoundVariableLists from "../theorem/steps/BoundVariableLists";

export default function PropertyOnTypeDefinitionDescription({propertyDefinition}) {
  const entryContext = useContext(EntryContext);

  const typeDefinition = propertyDefinition.parentType;
  const qualifier = propertyDefinition.requiredParentQualifier || typeDefinition.defaultQualifier;
  const requiredObjects = propertyDefinition.requiredParentObjects?.objectDefinitions ?? [];
  const requiredObjectsText = requiredObjects.length ?
    <>with {joinAsList(requiredObjects.map(o => o.name + " " + o.defaultTermName))}</> :
    "";
  const requiredObjectVariableNames = _.map(requiredObjects, o => o.defaultTermName);
  const termNames = [typeDefinition.defaultTermName, ...[qualifier ? qualifier.defaultTermNames : []]];

  const definingStatementElement = <BoundVariableLists.AddMultiple variables={_.map(requiredObjectVariableNames, n => [n])}>
    <CopiableExpression expression={propertyDefinition.definingStatement} splitConjunction/>
  </BoundVariableLists.AddMultiple>;

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(propertyDefinition.definingStatement, termNames, entryContext)}>
    {typeDefinition.article.capitalize()} {typeDefinition.name} {typeDefinition.defaultTermName} {formatQualifier(qualifier)} {requiredObjectsText} is <u>{propertyDefinition.name}</u> if {definingStatementElement}.
  </DisplayContext.Provider>;
}
