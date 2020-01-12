import React, {useContext} from "react";
import {formatHtml} from "../../helpers/Formatter";
import {InferenceSummary} from "../../InferenceSummary";
import ChapterContext from "./ChapterContext";
import ChapterEntryWrapper from "./ChapterEntryWrapper";
import DeleteEntryButton from "./DeleteEntryButton";

export default function InferenceEntry({title, entry, incomplete}) {
  return <ChapterEntryWrapper title={<>{title}: {formatHtml(entry.name)}</>}
                              url={entry.url}
                              incomplete={incomplete}>
    <InferenceSummary inference={entry}/>
  </ChapterEntryWrapper>;
};
