import React from "react";
import {CopiableExpression} from "../../ExpressionComponent";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

export default function DefinitionEntry({entry, children}) {
  return <ChapterEntryWrapper title={<>Definition: <CopiableExpression expression={entry.defaultValue} /></>}
                              url={entry.url}>
    {children}
  </ChapterEntryWrapper>;
}
