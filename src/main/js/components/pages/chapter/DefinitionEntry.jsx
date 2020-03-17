import React from "react";
import {CopiableExpression} from "../../ExpressionComponent";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

export default function DefinitionEntry({title, entry, children}) {
  return <ChapterEntryWrapper title={<>{title}: <CopiableExpression expression={entry.defaultValue} /></>}
                              url={entry.url}>
    {children}
  </ChapterEntryWrapper>;
}
