import React from "react";
import styled from "styled-components";
import {Breadcrumbs} from "./Breadcrumbs"
import {InferenceSummary} from "./InferenceSummary";
import {Monospace} from "./Monospace";
import {NavLinks} from "./NavLinks";
import {Page} from "./Page";


class InferenceHeader extends React.Component {
  render() {
    const {inference, title, className} = this.props;
    return <div className={className}>
      <h3 className="text-center mb-0">{title}: {inference.name}</h3>
      <Monospace className="text-center">{inference.id}</Monospace>
    </div>;
  }
}

const UsageList = styled.p`
  span:not(:last-child)::after {
      content: ' | ';
  }
`;

export class Inference extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      highlightedPremises: []
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  render() {
    const {inference, title, previous, next, usages, children, createPremiseElement} = this.props;
    return <Page breadcrumbs={<Breadcrumbs.Entry entryKey={inference.key}/>}>
      <NavLinks previous={previous} next={next}/>
      <InferenceHeader inference={inference} title={title} />
      <InferenceSummary createPremiseElement={createPremiseElement} inference={inference} highlightedPremises={this.state.highlightedPremises}/>
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
