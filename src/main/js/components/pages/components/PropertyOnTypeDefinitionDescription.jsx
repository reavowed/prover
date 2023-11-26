import React, {useContext} from "react";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import DisplayContext from "../../DisplayContext";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import AddBoundVariableLists from "../../expressions/boundVariables/AddBoundVariableLists";
import {formatQualifier} from "../../helpers/Formatter";
import {joinAsList} from "../../helpers/reactFunctions";

export default function PropertyOnTypeDefinitionDescription({propertyDefinition}) {
  const availableEntries = useContext(AvailableEntriesContext);

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

  const definingStatementElement = <AddBoundVariableLists variableLists={_.map(requiredObjectVariableNames, n => [n])}>
    <CopiableExpression expression={propertyDefinition.definingStatement} splitConjunction/>
  </AddBoundVariableLists>;

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(propertyDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {typeDefinition.article.capitalize()} {typeDefinition.name} {typeDefinition.mainVariableDefinition.name} {formatQualifier(qualifier)} {requiredObjectsText} is <u>{propertyDefinition.name}</u> if {definingStatementElement}.
  </DisplayContext.Provider>;
}
