import path from "path";
import React from "react";
import _ from "lodash";
import {Expression} from "../models/Expression";
import {Step} from "../models/Step";
import {HighlightableExpression} from "./ExpressionComponent";
import {Inference} from "./Inference";
import {Steps} from "./steps/Steps";

class Premise extends React.Component {
  render() {
    return <HighlightableExpression reference={{premiseIndex: this.props.index}} theoremContext={this.props.theoremContext} expression={this.props.premise} boundVariableLists={[]}/>;
  }
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.steps = {};
    this.state = {
      theorem: this.parseTheorem(props.theorem),
      highlightedPremises: [],
      highlightedConclusion: null
    }
  }

  registerStep = (step, path) => {
    this.steps[path.join(".")] = step;
  };
  unregisterStep = (path) => {
    delete this.steps[path.join(".")];
  };
  callOnStep = (path, action) => {
    this.steps[path.join(".")][action]();
  };

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  setHighlightedConclusion = (conclusion) => {
    this.setState({highlightedConclusion: conclusion});
  };

  fetchJsonForStep = (stepPath, childPath, options) => {
    const combinedPath = path.join(this.props.url, stepPath.join("."), childPath) + (childPath === "" ? "/" : "");
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

  updateTheorem = (result) => {
    _.merge(window.inferences, result.newInferences);
    const theorem = this.parseTheorem(result.theorem);
    this.setState({theorem: theorem});
  };

  render() {
    const {theorem} = this.state;
    const theoremContext = {
      highlightedPremises: this.state.highlightedPremises,
      highlightedConclusion: this.state.highlightedConclusion,
      setHighlightedPremises: this.setHighlightedPremises,
      setHighlightedConclusion: this.setHighlightedConclusion,
      fetchJsonForStep: this.fetchJsonForStep,
      updateTheorem: this.updateTheorem,
      registerStep: this.registerStep,
      unregisterStep: this.unregisterStep,
      callOnStep: this.callOnStep
    };
    const createPremiseElement = (premise, index) => {
      return <Premise premise={premise} index={index} theoremContext={theoremContext}/>
    };

    return <Inference inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" {...this.props}>
      <hr/>
      <h4>Proof</h4>
      <Steps steps={theorem.proof}
             path={[]}
             boundVariableLists={[]}
             theoremContext={theoremContext}/>
    </Inference>;
  }
}
