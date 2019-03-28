import path from "path";
import React from "react";
import {Expression} from "../models/Expression";
import {Step} from "../models/Step";
import _ from "../Parser";
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
      theorem: this.parseTheorem(props.theorem),
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

  fetchJsonForStep = (stepPath, childPath, options) => {
    const combinedPath = path.join(this.state.theorem.key.url, stepPath.join("."), childPath) + (childPath === "" ? "/" : "");
    return window.fetch(combinedPath, options)
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.statusText;
        }
      });
  };

  parseTheorem = (theoremJson) => {
    return {
      name: theoremJson.name,
      id: theoremJson.id,
      key: theoremJson.key,
      premises: theoremJson.premises.map(Expression.parseFromJson),
      conclusion: Expression.parseFromJson(theoremJson.conclusion),
      proof: Step.parseFromJson(theoremJson.proof)
    };
  };

  updateTheorem = (theoremJSON) => {
    const theorem = this.parseTheorem(theoremJSON);
    this.setState({theorem: theorem});
  };

  render() {
    const {theorem} = this.state;
    const highlighting = {
      highlightedPremises: this.state.highlightedPremises,
      highlightedConclusion: this.state.highlightedConclusion,
      setHighlightedPremises: this.setHighlightedPremises,
      setHighlightedConclusion: this.setHighlightedConclusion
    };
    const apiService = {
      fetchJsonForStep: this.fetchJsonForStep,
      updateTheorem: this.updateTheorem
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
             apiService={apiService}/>
    </Inference>;
  }
}
