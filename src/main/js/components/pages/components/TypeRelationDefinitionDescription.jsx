import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import AvailableEntries from "../../AvailableEntries";
import {CopiableExpression} from "../../ExpressionComponent";
import {joinWordElements} from "../../helpers/reactFunctions";

export default function TypeRelationDefinitionDescription({typeRelationDefinition}) {
  const availableEntries = useContext(AvailableEntries);
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

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(typeRelationDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {joinWordElements(words)}.
  </DisplayContext.Provider>;
}
