import React, {useContext} from "react";
import DisplayContext from "../../DisplayContext";
import EntryContext from "../../EntryContext";
import {formatHtml} from "../../helpers/Formatter";
import {InferenceSummary} from "../../InferenceSummary";
import ChapterEntryWrapper from "./ChapterEntryWrapper";

export default function InferenceEntry({type, url, inference, incomplete}) {
  return <ChapterEntryWrapper title={<>{type}: {formatHtml(inference.name)}</>}
                         url={url}
                         incomplete={incomplete}>
    <InferenceSummary inference={inference}/>
  </ChapterEntryWrapper>;
};
