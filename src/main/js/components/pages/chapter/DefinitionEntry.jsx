import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import AvailableEntries from "../../AvailableEntries";
import {CopiableExpression} from "../../ExpressionComponent";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

export default function DefinitionEntry({definition, url, children}) {
  const availableEntries = useContext(AvailableEntries);
  return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(definition, availableEntries)}>
      <ChapterEntryWrapper title={<>Definition: <CopiableExpression expression={definition.defaultValue} /></>}
                           url={url}>
      {children}
    </ChapterEntryWrapper>
  </DisplayContext.Provider>;
}
