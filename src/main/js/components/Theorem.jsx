import path from "path";
import React from "react";
import {Parser} from "../Parser";
import {Breadcrumbs} from "./Breadcrumbs"
import {HighlightableStatement} from "./Expression";
import {Page} from "./Page";
import {InferenceSummary} from "./InferenceSummary";
import {Monospace} from "./Monospace";
import {Steps} from "./steps/Steps";
import {NavLinks} from "./NavLinks";

class Premise extends React.Component {
  render() {
    return <HighlightableStatement reference={"p" + this.props.index} highlightedPremises={this.props.highlightedPremises} statement={this.props.premise} boundVariableLists={[]}/>;
  }
}

class TheoremHeader extends React.Component {
  render() {
    const {theorem, className} = this.props;
    return <div className={className}>
      <h3 className="text-center mb-0">Theorem: {theorem.name}</h3>
      <Monospace className="text-center">{theorem.id}</Monospace>
    </div>;
  }
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      theorem: Parser.parseTheorem(props.theorem),
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
    const combinedPath = path.join(this.state.theorem.key.url, stepPath.join("."), childPath) + (childPath === "" ? "/" : "");
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
    return <Page breadcrumbs={<Breadcrumbs.Entry entryKey={theorem.key}/>}>
      <NavLinks previous={previousEntry} next={nextEntry}/>

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
            <div key={usageChapter.key.url}>
              <h6>{usageBook.title} - {usageChapter.title}</h6>
              <p>{theorems.map(theorem => <span className="usage" key={theorem.key.url}> <a className="usageLink" href={theorem.key.url}>{theorem.name}</a> </span>)}</p>
            </div>
          )}
        </div>
      }
    </Page>;
  }
}
