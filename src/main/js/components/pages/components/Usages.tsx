import React from "react";
import styled from "styled-components";
import {formatHtml} from "../../helpers/Formatter";
import Monospace from "../../Monospace";
import {LinkSummary} from "../../definitions/LinkSummary";
import _ from "lodash";

const UsageList = styled.p`
  span:not(:last-child)::after {
      content: ' | ';
  }
`;

export type ChapterUsages = {
  bookTitle: string
  chapterTitle: string
  theoremUsages: TheoremUsages[]
}
type TheoremUsages = {
  link: LinkSummary
  inferenceIds: string[]
}
type UsagesProps = {
  usages: ChapterUsages[]
  title?: string
}
function Usages({usages, title}: UsagesProps) {
  return usages.length > 0 ? <>
    <hr/>
    {title && <h5>{title}</h5>}
    {usages.map(({bookTitle, chapterTitle, theoremUsages}) =>
      <div key={bookTitle + "-" + chapterTitle}>
        <h6>{bookTitle} - {chapterTitle}</h6>
        <UsageList>{theoremUsages.map(({link, inferenceIds}) =>
          <Monospace.Text key={link.url}>
            <Monospace.Link className="usageLink" href={link.url + "#inferencesToHighlight=" + inferenceIds.join(",")}>
              {formatHtml(link.title)}
            </Monospace.Link>
          </Monospace.Text>)
        }</UsageList>
      </div>
    )}
  </> : null;
}

type UsagesForInferenceProps = UsagesProps & {
  inferenceId: string
}
function ForInference({usages, inferenceId, title}: UsagesForInferenceProps) {
  function filterTheoremLinks(theoremUsages: TheoremUsages[]): TheoremUsages[] {
    return _.chain(theoremUsages)
      .filter(({inferenceIds}) => inferenceIds.includes(inferenceId))
      .map(({link}) => {return {link, inferenceIds: [inferenceId]}})
      .value();
  }
  function filterUsages(usages: ChapterUsages[]): ChapterUsages[] {
    return _.chain(usages)
      .map(({bookTitle, chapterTitle, theoremUsages}) => {return {bookTitle, chapterTitle, theoremUsages: filterTheoremLinks(theoremUsages)}})
      .filter(({theoremUsages}) => theoremUsages.length > 0)
      .value();
  }

  return <Usages usages={filterUsages(usages)} title={title}/>
};

export default Object.assign(Usages, {
  ForInference
});
