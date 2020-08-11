import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import {joinWordElements} from "../../helpers/reactFunctions";

export default function TypeRelationDefinitionDescription({typeRelationDefinition}) {
  const entryContext = useContext(EntryContext);
  const variableDefinitions = [typeRelationDefinition.firstVariable, typeRelationDefinition.secondVariable];
  const words = [
    typeRelationDefinition.firstType.article.capitalize(),
    typeRelationDefinition.firstType.name,
    typeRelationDefinition.firstVariable.name,
    formatQualifier(typeRelationDefinition.firstType.defaultQualifier, typeRelationDefinition.firstQualifierTerms.map(e => <CopiableExpression expression={e}/>)),
    <u>{typeRelationDefinition.linkingPhrase}</u>,
    (typeRelationDefinition.firstType.symbol === typeRelationDefinition.secondType.symbol) ? "another" : typeRelationDefinition.secondType.article,
    typeRelationDefinition.secondType.name,
    typeRelationDefinition.secondVariable.name,
    formatQualifier(typeRelationDefinition.secondType.defaultQualifier, typeRelationDefinition.secondQualifierTerms.map(e => <CopiableExpression expression={e}/>)),
    "if",
    <CopiableExpression expression={typeRelationDefinition.definingStatement} splitConjunction/>
  ];

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeRelationDefinition.definingStatement, variableDefinitions, entryContext)}>
    {joinWordElements(words)}.
  </DisplayContext.Provider>;
}
