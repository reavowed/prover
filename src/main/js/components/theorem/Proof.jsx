import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import {Steps} from "../steps/Steps";
import {FetchJsonAndUpdate} from "./TheoremStore";
import ProofContext from "./ProofContext";

export default class Proof extends React.Component {
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

  delete = () => {
    this.props.dispatch(FetchJsonAndUpdate(path.join("proofs", this.props.index.toString()), {method: "DELETE"}));
  };

  render() {
    const {title, index, steps, deleteable} = this.props;

    const proofContext = {
      proofIndex: index,
      registerStep: this.registerStep,
      unregisterStep: this.unregisterStep,
      callOnStep: this.callOnStep
    };
    return <>
      <hr/>
      {deleteable && <Button onClick={this.delete} variant="danger" size="sm" className="float-right"><i className="fas fa-times"/></Button>}
      <h4>{title}</h4>
      <ProofContext.Provider value={proofContext}>
        <Steps steps={steps}
               path={[]}
               boundVariableLists={[]}/>
      </ProofContext.Provider>
    </>;
  }
}
