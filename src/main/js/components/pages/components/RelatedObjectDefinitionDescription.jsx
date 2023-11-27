import React, {useContext} from "react";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "../../DisplaySettings";
import AddBoundVariableLists from "../../expressions/boundVariables/AddBoundVariableLists";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function RelatedObjectDefinitionDescription({relatedObjectDefinition}) {
  const availableEntries = useContext(AvailableEntriesContext);
  const typeDefinition = relatedObjectDefinition.parentTypeConditions.parentType;
  const qualifier = relatedObjectDefinition.parentTypeConditions.requiredParentQualifier ?
    relatedObjectDefinition.parentTypeConditions.requiredParentQualifier.qualifier :
    typeDefinition.defaultQualifier;
  const requiredObjects = relatedObjectDefinition.parentTypeConditions.requiredParentObjects?.objectDefinitions ?? [];
  const requiredObjectVariableNames = _.map(requiredObjects, o => o.mainVariableDefinition.name);
  const variableDefinitions = [relatedObjectDefinition.mainVariableDefinition, typeDefinition.mainVariableDefinition, ...(qualifier ? qualifier.variableDefinitions : [])];

  const definingStatementElement = <AddBoundVariableLists variableLists={_.map(requiredObjectVariableNames, n => [n])}>
    <CopiableExpression expression={relatedObjectDefinition.definingStatement} splitConjunction/>
  </AddBoundVariableLists>;

  return <DisplaySettingsContext.Provider value={DisplaySettings.forTypeLikeDefinition(relatedObjectDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {relatedObjectDefinition.article.capitalize()} <u>{relatedObjectDefinition.name}</u> for {typeDefinition.article} {typeDefinition.name} {typeDefinition.mainVariableDefinition.name} {formatQualifier(qualifier)} is an object {relatedObjectDefinition.mainVariableDefinition.name} such that {definingStatementElement}.
  </DisplaySettingsContext.Provider>;
}
