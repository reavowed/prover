import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {joinWordElements} from "../../helpers/reactFunctions";

export default function TypeRelationDefinitionDescription({typeRelationDefinition}) {
  const entryContext = useContext(EntryContext);
  const variableDefinitions = [typeRelationDefinition.firstVariable, typeRelationDefinition.secondVariable];
  const words = [
    typeRelationDefinition.firstType.article.capitalize(),
    typeRelationDefinition.firstType.name,
    typeRelationDefinition.firstVariable.name,
    <u>{typeRelationDefinition.linkingPhrase}</u>,
    (typeRelationDefinition.firstType.symbol === typeRelationDefinition.secondType.symbol) ? "another" : typeRelationDefinition.secondType.article,
    typeRelationDefinition.secondType.name,
    typeRelationDefinition.secondVariable.name,
    "if",
    <CopiableExpression expression={typeRelationDefinition.definingStatement} splitConjunction/>
  ];

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeRelationDefinition.definingStatement, variableDefinitions, entryContext)}>
    {joinWordElements(words)}
  </DisplayContext.Provider>;
}
