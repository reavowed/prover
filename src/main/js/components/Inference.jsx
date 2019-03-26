import React from "react";
import styled from "styled-components";
import {Breadcrumbs} from "./Breadcrumbs"
import {InferenceSummary} from "./InferenceSummary";
import {Monospace} from "./Monospace";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";

const UsageList = styled.p`
  span:not(:last-child)::after {
      content: ' | ';
  }
`;

export class Inference extends React.Component {
  render() {
    const {inference, title, previous, next, usages, children, createPremiseElement} = this.props;
    return <Page breadcrumbs={<Breadcrumbs.Entry entryKey={inference.key}/>}>
      <NavLinks previous={previous} next={next}/>
      <h3 className="text-center mb-0">{title}: {inference.name}</h3>
      <Monospace className="text-center">{inference.id}</Monospace>
      <InferenceSummary createPremiseElement={createPremiseElement} inference={inference}/>
      {children}
      {usages.length > 0 &&
      <div>
        <hr />
        {usages.map(([usageBook, usageChapter, theorems]) =>
          <div key={usageChapter.key.url}>
            <h6>{usageBook.title} - {usageChapter.title}</h6>
            <UsageList>{theorems.map(theorem => <Monospace.Text><Monospace.Link className="usageLink" href={theorem.key.url}>{theorem.name}</Monospace.Link></Monospace.Text>)}</UsageList>
          </div>
        )}
      </div>
      }
    </Page>;
  }
}
