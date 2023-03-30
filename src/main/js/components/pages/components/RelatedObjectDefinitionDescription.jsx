import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import AvailableEntries from "../../AvailableEntries";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import BoundVariableLists from "../theorem/steps/BoundVariableLists";

export default function RelatedObjectDefinitionDescription({relatedObjectDefinition}) {
  const availableEntries = useContext(AvailableEntries);
  const typeDefinition = relatedObjectDefinition.parentTypeConditions.parentType;
  const qualifier = relatedObjectDefinition.parentTypeConditions.requiredParentQualifier ?
    relatedObjectDefinition.parentTypeConditions.requiredParentQualifier.qualifier :
    typeDefinition.defaultQualifier;
  const requiredObjects = relatedObjectDefinition.parentTypeConditions.requiredParentObjects?.objectDefinitions ?? [];
  const requiredObjectVariableNames = _.map(requiredObjects, o => o.mainVariableDefinition.name);
  const variableDefinitions = [relatedObjectDefinition.mainVariableDefinition, typeDefinition.mainVariableDefinition, ...(qualifier ? qualifier.variableDefinitions : [])];

  const definingStatementElement = <BoundVariableLists.AddMultiple variables={_.map(requiredObjectVariableNames, n => [n])}>
    <CopiableExpression expression={relatedObjectDefinition.definingStatement} splitConjunction/>
  </BoundVariableLists.AddMultiple>;

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(relatedObjectDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {relatedObjectDefinition.article.capitalize()} <u>{relatedObjectDefinition.name}</u> for {typeDefinition.article} {typeDefinition.name} {typeDefinition.mainVariableDefinition.name} {formatQualifier(qualifier)} is an object {relatedObjectDefinition.mainVariableDefinition.name} such that {definingStatementElement}.
  </DisplayContext.Provider>;
}
