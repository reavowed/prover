import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

export default function DefinitionEntry({definition, url, children}) {
  const availableEntries = useContext(AvailableEntriesContext);
  return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(definition, availableEntries)}>
      <ChapterEntryWrapper title={<>Definition: <CopiableExpression expression={definition.defaultValue} /></>}
                           url={url}>
      {children}
    </ChapterEntryWrapper>
  </DisplayContext.Provider>;
}
