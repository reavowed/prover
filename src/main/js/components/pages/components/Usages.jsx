import React from "react";
import styled from "styled-components";
import {formatHtml} from "../../helpers/Formatter";
import {Monospace} from "../../Monospace";

const UsageList = styled.p`
  span:not(:last-child)::after {
      content: ' | ';
  }
`;

export function Usages({usages, title}) {
  return usages.length > 0 && <>
    <hr/>
    {title && <h5>{title}</h5>}
    {usages.map(([bookTitle, chapterTitle, theoremLinks]) =>
      <div key={bookTitle + "-" + chapterTitle}>
        <h6>{bookTitle} - {chapterTitle}</h6>
        <UsageList>{theoremLinks.map(([link, inferenceIds]) =>
          <Monospace.Text key={link.url}>
            <Monospace.Link className="usageLink" href={link.url + "#inferencesToHighlight=" + inferenceIds.join(",")}>
              {formatHtml(link.title)}
            </Monospace.Link>
          </Monospace.Text>)
        }</UsageList>
      </div>
    )}
  </>;
}

Usages.ForInference = function({usages, inferenceId, title}) {

  function filterTheoremLinks(theoremLinks) {
    return _.chain(theoremLinks)
      .filter(([,inferenceIds]) => inferenceIds.includes(inferenceId))
      .map(([link,]) => [link, [inferenceId]])
      .value();
  }
  function filterUsages(usages) {
    return _.chain(usages)
      .map(([bookTitle, chapterTitle, theoremLinks]) => [bookTitle, chapterTitle, filterTheoremLinks(theoremLinks)])
      .filter(([,,theoremLinks]) => theoremLinks.length > 0)
      .value();
  }

  return <>
    <Usages usages={filterUsages(usages)} title={title}/>
  </>
};
