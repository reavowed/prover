import React, {useContext} from "react";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";

export default function PropertyOnTypeDefinitionDescription({symbol, parentTypeSymbol, definingStatement}) {
  const entryContext = useContext(EntryContext);
  const typeDefinition = entryContext.typeDefinitions[parentTypeSymbol];
  const propertyDefinition = _.find(typeDefinition.properties, p => p.symbol === symbol);
  const qualifier = propertyDefinition.requiredParentQualifier ?
    _.find(typeDefinition.qualifiers, q => q.symbol === propertyDefinition.requiredParentQualifier).qualifier :
    typeDefinition.defaultQualifier;

  return <>
    {typeDefinition.article.capitalize()} {typeDefinition.name} {typeDefinition.defaultTermName} {formatQualifier(qualifier)} is {propertyDefinition.name} if <CopiableExpression expression={definingStatement} splitConjunction/>.
  </>;
}
