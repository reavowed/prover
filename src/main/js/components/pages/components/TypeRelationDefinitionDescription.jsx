import React, {useContext} from "react";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "../../DisplaySettings";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import {joinWordElements} from "../../helpers/reactFunctions";

export default function TypeRelationDefinitionDescription({typeRelationDefinition}) {
  const availableEntries = useContext(AvailableEntriesContext);
  const variableDefinitions = [typeRelationDefinition.firstVariable, typeRelationDefinition.secondVariable, ...(typeRelationDefinition.qualifierVariableDefinitions || [])];
  const words = [
    typeRelationDefinition.firstType.article.capitalize(),
    typeRelationDefinition.firstType.name,
    typeRelationDefinition.firstVariable.name,
    // If both qualifiers say the same thing, only state it once at the end
    (typeRelationDefinition.firstQualifierText !== typeRelationDefinition.secondQualifierText) ? typeRelationDefinition.firstQualifierText : null,
    <u>{typeRelationDefinition.linkingPhrase}</u>,
    (typeRelationDefinition.firstType.symbol === typeRelationDefinition.secondType.symbol) ? "another" : typeRelationDefinition.secondType.article,
    typeRelationDefinition.secondType.name,
    typeRelationDefinition.secondVariable.name,
    typeRelationDefinition.secondQualifierText,
    "if",
    <CopiableExpression expression={typeRelationDefinition.definingStatement} splitConjunction/>
  ];

  return <DisplaySettingsContext.Provider value={DisplaySettings.forTypeLikeDefinition(typeRelationDefinition.definingStatement, variableDefinitions, availableEntries)}>
    {joinWordElements(words.filter(w => w !== null && w !== undefined))}.
  </DisplaySettingsContext.Provider>;
}
