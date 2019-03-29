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
    return usages.map(([usageBook, usageChapter, theorems]) =>
      <div key={usageChapter.key.url}>
        <h6>{usageBook.title} - {usageChapter.title}</h6>
        <UsageList>{theorems.map(theorem => <Monospace.Text key={theorem.key.url}><Monospace.Link className="usageLink" href={theorem.key.url}>{theorem.name}</Monospace.Link></Monospace.Text>)}</UsageList>
      </div>
    );
}
}
