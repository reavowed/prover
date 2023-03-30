import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import AvailableEntries from "../../AvailableEntries";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import {joinAsList} from "../../helpers/reactFunctions";
import BoundVariableLists from "../theorem/steps/BoundVariableLists";

export default function PropertyOnTypeDefinitionDescription({propertyDefinition}) {
  const availableEntries = useContext(AvailableEntries);

  const typeDefinition = propertyDefinition.parentTypeConditions.parentType;
  const qualifier = propertyDefinition.parentTypeConditions.requiredParentQualifier ?
    propertyDefinition.parentTypeConditions.requiredParentQualifier.qualifier :
    typeDefinition.defaultQualifier;
  const requiredObjects = propertyDefinition.parentTypeConditions.requiredParentObjects?.objectDefinitions ?? [];
  const requiredObjectsText = requiredObjects.length ?
    <>with {joinAsList(requiredObjects.map(o => o.name + " " + o.mainVariableDefinition.name))}</> :
    "";
  const requiredObjectVariableNames = _.map(requiredObjects, o => o.mainVariableDefinition.name);
  const variableDefinitions = [typeDefinition.mainVariableDefinition, ...(qualifier ? qualifier.variableDefinitions : [])];

  const definingStatementElement = <BoundVariableLists.AddMultiple variables={_.map(requiredObjectVariableNames, n => [n])}>
    <CopiableExpression expression={propertyDefinition.definingStatement} splitConjunction/>
  </BoundVariableLists.AddMultiple>;

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(propertyDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {typeDefinition.article.capitalize()} {typeDefinition.name} {typeDefinition.mainVariableDefinition.name} {formatQualifier(qualifier)} {requiredObjectsText} is <u>{propertyDefinition.name}</u> if {definingStatementElement}.
  </DisplayContext.Provider>;
}
