import path from "path";
import React from "react";
import styled from "styled-components";
import {Parser} from "../Parser";
import {HighlightableStatement} from "./Expression";
import {InferenceSummary} from "./InferenceSummary";
import {Steps} from "./steps/Steps";

class Premise extends React.Component {
  render() {
    return <HighlightableStatement reference={"p" + this.props.index} highlightedPremises={this.props.highlightedPremises} statement={this.props.premise} boundVariableLists={[]}/>;
  }
}

const TheoremHeader = styled(class extends React.Component {
  render() {
    const {theorem, className} = this.props;
    return <div className={className}>
      <h3>
        Theorem: {theorem.name}
      </h3>
      <div className="inferenceId">
        {theorem.id}
      </div>
    </div>;
  }
})`
  margin-bottom: 1rem;
`;

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      theorem: props.theorem,
      highlightedPremises: []
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  createPremiseElement = (premise, index) => {
    return <Premise premise={premise} index={index} highlightedPremises={this.state.highlightedPremises}/>
  };

  fetchForStep = (stepPath, childPath, options) => {
    if (_.isObject(childPath)) {
      options = childPath;
      childPath = "";
    }
    const combinedPath = path.join(this.state.theorem.key, stepPath.join("."), childPath) + (childPath === "" ? "/" : "");
    return window.fetch(combinedPath, options);
  };

  updateTheorem = (response) => {
    if (response.ok) {
      return response.json().then(theoremJSON => {
        const theorem = Parser.parseTheorem(theoremJSON);
        this.setState({theorem: theorem});
      });
    } else {
      throw response.statusText;
    }
  };

  render() {
    const {previousEntry, nextEntry, usages} = this.props;
    const {theorem} = this.state;
    return <div className="inference">
      <div className="navigationLinks">
        {previousEntry && <a className="navigationLink float-left" href={previousEntry.key}>&laquo; {previousEntry.name}</a>}
        {nextEntry && <a className="navigationLink float-right" href={nextEntry.key}>{nextEntry.name} &raquo;</a>}
      </div>

      <TheoremHeader theorem={theorem} />
      <InferenceSummary createPremiseElement={this.createPremiseElement} inference={theorem} highlightedPremises={this.state.highlightedPremises}/>

      <hr/>

      <h4>Proof</h4>
      <Steps steps={theorem.proof}
             path={[]}
             boundVariableLists={[]}
             setHighlightedPremises={this.setHighlightedPremises}
             highlightedPremises={this.state.highlightedPremises}
             fetchForStep={this.fetchForStep}
             updateTheorem={this.updateTheorem}/>

      {usages.length > 0 &&
        <div>
          <hr />
          {usages.map(([usageBook, usageChapter, theorems]) =>
            <div key={usageBook.key.value + "/" + usageChapter.key.value}>
              <h6>{usageBook.title} - {usageChapter.title}</h6>
              <p>{theorems.map(theorem => <span className="usage" key={theorem.key.value}> <a className="usageLink" href={theorem.key}>{theorem.name}</a> </span>)}</p>
            </div>
          )}
        </div>
      }
    </div>
  }
}
