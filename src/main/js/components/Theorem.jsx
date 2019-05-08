import path from "path";
import React from "react";
import _ from "lodash";
import {Expression} from "../models/Expression";
import {Step} from "../models/Step";
import {HighlightableExpression} from "./ExpressionComponent";
import {Inference} from "./Inference";
import {Steps} from "./steps/Steps";
import Button from "react-bootstrap/Button";

class Premise extends React.Component {
  render() {
    return <HighlightableExpression reference={{premiseIndex: this.props.index}} theoremContext={this.props.theoremContext} expression={this.props.premise} boundVariableLists={[]}/>;
  }
}

class Proof extends React.Component {
  constructor(props) {
    super(props);
    this.steps = {};
    this.state = {
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

  setHighlightedConclusion = (conclusion) => {
    this.setState({highlightedConclusion: conclusion});
  };

  fetchJsonForStep = (stepPath, childPath, options) => {
    const combinedPath = path.join("proofs", this.props.index.toString(), stepPath.join("."), childPath) + (childPath === "" ? "/" : "");
    return this.props.fetchJson(combinedPath, options);
  };

  delete = () => {
    this.props.fetchJson(path.join("proofs", this.props.index.toString()), {method: "DELETE"})
      .then(this.props.updateTheorem);
  };

  render() {
    const {title, steps, highlightedPremises, setHighlightedPremises, updateTheorem, deleteable} = this.props;
    const theoremContext = {
      highlightedPremises: highlightedPremises,
      highlightedConclusion: this.state.highlightedConclusion,
      setHighlightedPremises: setHighlightedPremises,
      setHighlightedConclusion: this.setHighlightedConclusion,
      fetchJsonForStep: this.fetchJsonForStep,
      updateTheorem: updateTheorem,
      registerStep: this.registerStep,
      unregisterStep: this.unregisterStep,
      callOnStep: this.callOnStep
    };
    return <>
      <hr/>
      {deleteable && <Button onClick={this.delete} variant="danger" size="sm" className="float-right"><i className="fas fa-times"/></Button>}
      <h4>{title}</h4>
      <Steps steps={steps}
             path={[]}
             boundVariableLists={[]}
             theoremContext={theoremContext}/>
    </>;
  }
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.steps = {};
    this.state = {
      theorem: this.parseTheorem(props.theorem),
      highlightedPremises: []
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  parseTheorem = (theoremJson) => {
    return {
      name: theoremJson.name,
      id: theoremJson.id,
      key: theoremJson.key,
      premises: theoremJson.premises.map(Expression.parseFromJson),
      conclusion: Expression.parseFromJson(theoremJson.conclusion),
      proofs: theoremJson.proofs.map(proof => Step.parseFromJson(proof.steps))
    };
  };

  updateTheorem = (result) => {
    _.merge(window.inferences, result.newInferences);
    const theorem = this.parseTheorem(result.theorem);
    this.setState({theorem: theorem});
  };

  fetchJson = (subpath, options) => {
    return window.fetch(path.join(this.props.url, subpath), options)
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.statusText;
        }
      });
  };

  addProof = () => {
    this.fetchJson("proofs", {method: "POST"}).then(this.updateTheorem);
  };

  render() {
    const {url} = this.props;
    const {theorem, highlightedPremises} = this.state;

    const createPremiseElement = (premise, index) => {
      return <Premise premise={premise} index={index} theoremContext={{highlightedPremises: highlightedPremises}}/>
    };

    function getProofTitle(index) {
      return theorem.proofs.length > 1 ? `Proof ${index + 1}` : "Proof";
    }

    return <Inference inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" {...this.props}>
      {theorem.proofs.map((proof, index) =>
          <Proof key={index}
                 title={getProofTitle(index)}
                 steps={proof}
                 index={index}
                 theoremUrl={url}
                 highlightedPremises={highlightedPremises}
                 setHighlightedPremises={this.setHighlightedPremises}
                 fetchJson={this.fetchJson}
                 updateTheorem={this.updateTheorem}
                 deleteable={theorem.proofs.length > 1}/>)
      }
      <Button size="sm" className="mt-3" onClick={this.addProof}>Add proof</Button>
    </Inference>;
  }
}
