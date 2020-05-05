import React, {useContext} from "react";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import {joinAsList} from "../../helpers/reactFunctions";
import BoundVariableLists from "../theorem/steps/BoundVariableLists";

export default function PropertyOnTypeDefinitionDescription({symbol, parentTypeSymbol, definingStatement}) {
  const entryContext = useContext(EntryContext);
  const typeDefinition = entryContext.typeDefinitions[parentTypeSymbol];
  const propertyDefinition = _.find(typeDefinition.properties, p => p.symbol === symbol);
  const qualifier = propertyDefinition.requiredParentQualifier ?
    _.find(typeDefinition.qualifiers, q => q.symbol === propertyDefinition.requiredParentQualifier).qualifier :
    typeDefinition.defaultQualifier;
  const requiredObjects = _.map(propertyDefinition.requiredParentObjects, s => _.find(typeDefinition.relatedObjects, o => o.symbol === s));
  const requiredObjectsText = requiredObjects.length ?
    <>with {joinAsList(requiredObjects.map(o => o.name + " " + o.defaultTermName))}</> :
    "";
  const requiredObjectVariableNames = _.map(requiredObjects, o => o.defaultTermName);

  const definingStatementElement = <BoundVariableLists.AddMultiple variables={_.map(requiredObjectVariableNames, n => [n])}>
    <CopiableExpression expression={definingStatement} splitConjunction/>
  </BoundVariableLists.AddMultiple>;

  return <>{typeDefinition.article.capitalize()} {typeDefinition.name} {typeDefinition.defaultTermName} {formatQualifier(qualifier)} {requiredObjectsText} is <u>{propertyDefinition.name}</u> if {definingStatementElement}.</>;
}
