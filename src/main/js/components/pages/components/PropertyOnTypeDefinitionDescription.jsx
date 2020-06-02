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
    <>with {joinAsList(requiredObjects.map(o => o.name + " " + o.mainVariableDefinition.name))}</> :
    "";
  const requiredObjectVariableNames = _.map(requiredObjects, o => o.mainVariableDefinition.name);
  const termNames = [typeDefinition.mainVariableDefinition.name, ...[qualifier ? qualifier.variableDefinitions.map(d => d.name) : []]];

  const definingStatementElement = <BoundVariableLists.AddMultiple variables={_.map(requiredObjectVariableNames, n => [n])}>
    <CopiableExpression expression={propertyDefinition.definingStatement} splitConjunction/>
  </BoundVariableLists.AddMultiple>;

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(propertyDefinition.definingStatement, termNames, entryContext)}>
    {typeDefinition.article.capitalize()} {typeDefinition.name} {typeDefinition.mainVariableDefinition.name} {formatQualifier(qualifier)} {requiredObjectsText} is <u>{propertyDefinition.name}</u> if {definingStatementElement}.
  </DisplayContext.Provider>;
}
