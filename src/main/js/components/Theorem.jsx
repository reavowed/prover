import path from "path";
import React from "react";
import {Parser} from "../Parser";
import {HighlightableStatement} from "./Expression";
import {Inference} from "./Inference";
import {Steps} from "./steps/Steps";

class Premise extends React.Component {
  render() {
    return <HighlightableStatement reference={"p" + this.props.index} highlightedPremises={this.props.highlightedPremises} statement={this.props.premise} boundVariableLists={[]}/>;
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
    const {theorem} = this.state;
    return <Inference inference={theorem} createPremiseElement={this.createPremiseElement} title="Theorem" {...this.props}>
      <hr/>
      <h4>Proof</h4>
      <Steps steps={theorem.proof}
             path={[]}
             boundVariableLists={[]}
             setHighlightedPremises={this.setHighlightedPremises}
             highlightedPremises={this.state.highlightedPremises}
             fetchForStep={this.fetchForStep}
             updateTheorem={this.updateTheorem}/>
    </Inference>;
  }
}
