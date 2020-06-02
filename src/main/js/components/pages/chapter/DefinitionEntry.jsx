import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {CopiableExpression} from "../../ExpressionComponent";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

export default function DefinitionEntry({definition, url, children}) {
  const entryContext = useContext(EntryContext);
  return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(definition, entryContext)}>
      <ChapterEntryWrapper title={<>Definition: <CopiableExpression expression={definition.defaultValue} /></>}
                           url={url}>
      {children}
    </ChapterEntryWrapper>
  </DisplayContext.Provider>;
}
