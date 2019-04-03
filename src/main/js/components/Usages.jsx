import React from "react";
import styled from "styled-components";
import {Monospace} from "./Monospace";

const UsageList = styled.p`
  span:not(:last-child)::after {
      content: ' | ';
  }
`;

export class Usages extends React.Component {
  render() {
    const {usages} = this.props;
    return usages.map(([bookTitle, chapterTitle, theoremLinks]) =>
      <div key={bookTitle + "-" + chapterTitle}>
        <h6>{bookTitle} - {chapterTitle}</h6>
        <UsageList>{theoremLinks.map(link => <Monospace.Text key={link.url}><Monospace.Link className="usageLink" href={link.url}>{link.title}</Monospace.Link></Monospace.Text>)}</UsageList>
      </div>
    );
}
}
