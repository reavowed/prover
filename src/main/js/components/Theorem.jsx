import path from "path";
import React from "react";
import {Parser} from "../Parser";
import {HighlightableExpression} from "./ExpressionComponent";
import {Inference} from "./Inference";
import {Steps} from "./steps/Steps";

class Premise extends React.Component {
  render() {
    return <HighlightableExpression reference={"p" + this.props.index} highlighting={this.props.highlighting} expression={this.props.premise} boundVariableLists={[]}/>;
  }
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      theorem: Parser.parseTheorem(props.theorem),
      highlightedPremises: [],
      highlightedConclusion: null
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  setHighlightedConclusion = (conclusion) => {
    this.setState({highlightedConclusion: conclusion});
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
    const highlighting = {
      highlightedPremises: this.state.highlightedPremises,
      highlightedConclusion: this.state.highlightedConclusion,
      setHighlightedPremises: this.setHighlightedPremises,
      setHighlightedConclusion: this.setHighlightedConclusion
    };
    const createPremiseElement = (premise, index) => {
      return <Premise premise={premise} index={index} highlighting={highlighting}/>
    };

    return <Inference inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" {...this.props}>
      <hr/>
      <h4>Proof</h4>
      <Steps steps={theorem.proof}
             path={[]}
             boundVariableLists={[]}
             highlighting={highlighting}
             fetchForStep={this.fetchForStep}
             updateTheorem={this.updateTheorem}/>
    </Inference>;
  }
}
