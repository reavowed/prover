import path from "path";
import React from "react";
import Button from "react-bootstrap/Button";
import {DndProvider} from "react-dnd";
import Backend from "react-dnd-html5-backend";
import {DeductionStep, GeneralizationStep, NamingStep, TargetStep} from "../../../models/Step";
import BoundVariableLists from "./steps/BoundVariableLists";
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
  unregisterStep = (step, path) => {
    if (this.steps[path.join(".")] === step) {
      delete this.steps[path.join(".")];
    }
  };
  callOnStep = (path, action) => {
    this.steps[path.join(".")][action]();
  };
  componentDidUpdate(prevProps, prevState, snapshot) {
    if (prevProps.steps !== this.props.steps) {
      _.forEach(_.values(this.steps), step => {
        if (step.onUpdate) {
          step.onUpdate();
        }
      });
    }
  }

  render() {
    const {title, index, steps, deleteable} = this.props;
    return <TheoremContext.Consumer>{theoremContext => {
      const proofContext = {
        parser: theoremContext.parser,
        variableDefinitions: theoremContext.variableDefinitions,
        registerStep: this.registerStep,
        unregisterStep: this.unregisterStep,
        callOnStep: this.callOnStep,
        fetchJson(subpath, options) {
          return theoremContext.fetchJson(path.join("proofs", index.toString(), subpath), options)
        },
        fetchJsonForStep(stepPath, subpath, options) {
          return this.fetchJson(path.join(stepPath.join("."), subpath), options)
        },
        fetchJsonForStepAndInsert(stepPath, subpath, options) {
          return this.fetchJsonForStep(stepPath, subpath, options)
            .then(stepJson => theoremContext.insertSteps(index, stepJson))
        },
        fetchJsonForStepAndReplace(stepPath, subpath, options) {
          return this.fetchJsonForStep(stepPath, subpath, options)
            .then(stepJson => theoremContext.replaceStep(index, stepJson))
            .then(([, newSteps]) => {
              const targetIndexes = _.chain(newSteps)
                .map((step, index) => [step, index])
                .filter(([step,]) => step instanceof TargetStep)
                .map(([, index]) => index)
                .value();
              if (targetIndexes.length === 1) {
                const newPath = [...stepPath.slice(0, stepPath.length - 1), stepPath[stepPath.length - 1] + targetIndexes[0]];
                this.callOnStep(newPath, "startProving");
              }
            });
        },
        fetchJsonForStepAndInsertAndReplace(stepPath, subpath, options) {
          return this.fetchJsonForStep(stepPath, subpath, options)
            .then(stepJson => theoremContext.insertAndReplaceSteps(index, stepJson))
            .then(([insertionPath, insertionSteps,]) => {
              const targetIndexes = _.chain(insertionSteps)
                .map((step, index) => [step, index])
                .filter(([step,]) => step instanceof TargetStep)
                .map(([, index]) => index)
                .value();
              if (targetIndexes.length === 1) {
                const newPath = [...insertionPath.slice(0, insertionPath.length - 1), insertionPath[insertionPath.length - 1] + targetIndexes[0]];
                this.callOnStep(newPath, "startProving");
              }
            });
        },
        fetchJsonForStepAndInsertAndReplaceMultiple(stepPath, subpath, options) {
          return this.fetchJsonForStep(stepPath, subpath, options)
            .then(stepJson => theoremContext.insertAndReplaceMultipleSteps(index, stepJson))
            .then(([insertionPath, insertionSteps, replacementPath, replacementSteps]) => {
              const replacementTargetIndexes = _.chain(replacementSteps)
                .map((step, index) => [step, index])
                .filter(([step,]) => step instanceof TargetStep)
                .map(([, index]) => index)
                .value();
              const insertionTargetIndexes = _.chain(insertionSteps)
                .map((step, index) => [step, index])
                .filter(([step,]) => step instanceof TargetStep)
                .map(([, index]) => index)
                .value();
              if (replacementTargetIndexes.length === 1) {
                const newPath = [...replacementPath.slice(0, replacementPath.length - 1), replacementPath[replacementPath.length - 1] + insertionSteps.length + replacementTargetIndexes[0]];
                this.callOnStep(newPath, "startProving");
              } else if (replacementTargetIndexes.length === 0 && insertionTargetIndexes.length === 1) {
                const newPath = [...insertionPath.slice(0, insertionPath.length - 1), insertionPath[insertionPath.length - 1] + insertionTargetIndexes[0]];
                this.callOnStep(newPath, "startProving");
              }
            });
        },
        fetchJsonAndInsertAndDelete(subpath, options) {
          return this.fetchJson(subpath, options)
            .then(stepJson => theoremContext.insertAndDeleteSteps(index, stepJson));
        },
        fetchJsonForStepAndReplaceWithWrapping(stepPath, subpath, options) {
          return this.fetchJsonForStep(stepPath, subpath, options)
            .then(stepJson => theoremContext.replaceStep(index, stepJson))
            .then(([, newSteps]) => {
              if (newSteps.length) {
                let path = [...stepPath.slice(0, stepPath.length - 1), stepPath[stepPath.length - 1] + newSteps.length - 1];
                let step = newSteps[newSteps.length - 1];
                while (step instanceof GeneralizationStep || step instanceof DeductionStep || step instanceof NamingStep) {
                  path = [...path, 0];
                  step = step.substeps[0];
                }
                if (step instanceof TargetStep) {
                  this.callOnStep(path, "startProving");
                }
              }
            });
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
      _.forEach(proofContext, (value, key) => {
        if (_.isFunction(value)) {
          proofContext[key] = value.bind(proofContext);
        }
      });

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
          <BoundVariableLists.Provider value={[]}>
            <Steps.Container path={[]}>
              <Steps steps={steps}
                     path={[]} />
            </Steps.Container>
          </BoundVariableLists.Provider>
        </DndProvider>
      </ProofContext.Provider>
    }}</TheoremContext.Consumer>;
  }
}
