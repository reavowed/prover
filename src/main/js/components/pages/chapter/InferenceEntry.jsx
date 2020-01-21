import React from "react";
import {formatHtml} from "../../helpers/Formatter";
import {InferenceSummary} from "../../InferenceSummary";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

export default function InferenceEntry({title, entry, incomplete}) {
  return <ChapterEntryWrapper title={<>{title}: {formatHtml(entry.name)}</>}
                              url={entry.url}
                              incomplete={incomplete}>
    <InferenceSummary inference={entry}/>
  </ChapterEntryWrapper>;
};
