import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import {formatQualifier} from "../../helpers/Formatter";
import ChapterEntryWrapper from "../chapter/ChapterEntryWrapper";

export default function TypeQualifierDefinitionDescription({symbol, parentTypeSymbol, definingStatement}) {
  const entryContext = useContext(EntryContext);
  const typeDefinition = entryContext.typeDefinitions[parentTypeSymbol];
  const qualifierDefinition = _.find(typeDefinition.qualifiers, q => q.symbol === symbol);
  const termNames = [typeDefinition.defaultTermName, ...qualifierDefinition.qualifier.defaultTermNames];

  return <DisplayContext.Provider value={DisplayContext.forTypeLikeDefinition(definingStatement, termNames, entryContext)}>
    {typeDefinition.article.capitalize()} <u>{typeDefinition.name} {formatQualifier(qualifierDefinition.qualifier)}</u> is a {typeDefinition.name} {typeDefinition.defaultTermName} such that <CopiableExpression expression={definingStatement} splitConjunction/>.
  </DisplayContext.Provider>;
}
