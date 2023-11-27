import React, {useContext} from "react";
import AvailableEntriesContext from "../../AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "../../DisplaySettings";
import {CopiableExpression} from "../../expressions/ExpressionComponent";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

export default function DefinitionEntry({definition, url, children}) {
  const availableEntries = useContext(AvailableEntriesContext);
  return <DisplaySettingsContext.Provider value={DisplaySettings.forExpressionDefinition(definition, availableEntries)}>
      <ChapterEntryWrapper title={<>Definition: <CopiableExpression expression={definition.defaultValue} /></>}
                           url={url}>
      {children}
    </ChapterEntryWrapper>
  </DisplaySettingsContext.Provider>;
}
