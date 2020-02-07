import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import {DndProvider} from "react-dnd";
import Backend from "react-dnd-html5-backend";
import {Steps} from "./steps/Steps";
import ProofContext from "./ProofContext";
import TheoremContext from "./TheoremContext";

export default class Proof extends React.Component {
  constructor(props) {
    super(props);
    this.steps = {};
  }

  registerStep = (step, path) => {
    this.steps[path.join(".")] = step;
    if (step.onUpdate) step.onUpdate();
  };
  unregisterStep = (path) => {
    delete this.steps[path.join(".")];
  };
  callOnStep = (path, action) => {
    this.steps[path.join(".")][action]();
  };
  componentDidUpdate(prevProps, prevState, snapshot) {
    if (prevProps.steps !== this.props.steps) {
      _.forEach(_.values(this.steps), step => {
        if (step.onUpdate) step.onUpdate();
      });
    }
  }

  render() {
    const {title, index, steps, deleteable} = this.props;
    return <TheoremContext.Consumer>{theoremContext => {
      const proofContext = {
        parser: theoremContext.parser,
        registerStep: this.registerStep,
        unregisterStep: this.unregisterStep,
        callOnStep: this.callOnStep,
        fetchJson(subpath, options) {
          return theoremContext.fetchJson(path.join("proofs", index.toString(), subpath), options)
        },
        fetchJsonAndUpdateTheorem(subpath, options) {
          return this.fetchJson(subpath, options).then(stepJson => theoremContext.updateStep(index, stepJson));
        },
        fetchJsonForStep(stepPath, subpath, options) {
          return this.fetchJson(path.join(stepPath.join("."), subpath), options)
        },
        fetchJsonForStepAndUpdateTheorem(stepPath, subpath, options) {
          return this.fetchJsonForStep(stepPath, subpath, options).then(stepJson => theoremContext.updateStep(index, stepJson));
        },
        updateTheorem: theoremContext.updateTheorem,
        setHighlighting(newHighlightedPremises, newHighlightedConclusion) {
          theoremContext.setHighlighting(newHighlightedPremises, newHighlightedConclusion, index);
        },
        getHighlighting() {
          return theoremContext.getHighlighting(index);
        },
        setHighlightingAction(actionHighlights, staticHighlights) {
          theoremContext.setHighlightingAction(actionHighlights, staticHighlights, index);
        },
        clearHighlightingAction: theoremContext.clearHighlightingAction
      };

      const duplicateProof = () => {
        theoremContext.fetchJson("proofs", {method: "POST", body: this.props.index})
          .then(theoremContext.updateTheorem);
      };

      const deleteProof = () => {
        proofContext.fetchJson("", {method: "DELETE"})
          .then(proofContext.updateTheorem);
      };

      return <ProofContext.Provider value={proofContext}>
        <hr/>
        {deleteable && <Button onClick={deleteProof} variant="danger" size="sm" className="float-right ml-1"><i className="fas fa-times"/></Button>}
        <Button onClick={duplicateProof} variant="primary" size="sm" className="float-right ml-1"><i className="fas fa-copy"/></Button>
        <h4>{title}</h4>
        <DndProvider backend={Backend}>
          <Steps.Container path={[]}>
            <Steps steps={steps}
                   path={[]} />
          </Steps.Container>
        </DndProvider>
      </ProofContext.Provider>
    }}</TheoremContext.Consumer>;
  }
}
