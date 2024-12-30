import path from "path";
import React, {useContext, useEffect, useRef} from "react";
import Button from "react-bootstrap/Button";
import {DndProvider} from "react-dnd";
import Backend from "react-dnd-html5-backend";
import {DeductionStep, GeneralizationStep, NamingStep, Step, TargetStep} from "../../../models/Step";
import BoundVariableListContext from "../../expressions/boundVariables/BoundVariableListContext";
import Steps from "./steps/Steps";
import ProofContext, {ProofContextType, StepActions} from "./ProofContext";
import TheoremContext from "./TheoremContext";
import _ from "lodash";

type ProofProps = {
  title: string
  index: number
  steps: Step[]
  deleteable: boolean
}

function getTargetStepIndexes(steps: Step[]): number[] {
  return _.chain(steps)
    .map((step, index) => {return {step, index}})
    .filter(({step}) => step instanceof TargetStep)
    .map(({index}) => index)
    .value();
}

export default function Proof({title, index, steps, deleteable}: ProofProps) {
  const stepActions = useRef<{[key: string]: StepActions}>({});
  const registerStep = (step: StepActions, path: number[]) => {
    stepActions.current[path.join(".")] = step;
    if (step.onUpdate) step.onUpdate();
  };
  const unregisterStep = (step: StepActions, path: number[]) => {
    if (stepActions.current[path.join(".")] === step) {
      delete stepActions.current[path.join(".")];
    }
  };
  const callOnStep = (path: number[], action: string) => {
    stepActions.current[path.join(".")][action]();
  };
  useEffect(() => {
    _.forEach(_.values(stepActions.current), step => {
      if (step.onUpdate) {
        step.onUpdate();
      }
    });
  }, [steps]);
  const theoremContext = useContext(TheoremContext)!;
  const stepToHighlight = theoremContext.stepToHighlight && theoremContext.stepToHighlight.startsWith(index + ".") ?
    theoremContext.stepToHighlight.substring((index + ".").length) :
    undefined;
  const proofContext: ProofContextType = {
    index,
    parser: theoremContext.parser,
    variableDefinitions: theoremContext.variableDefinitions,
    stepToHighlight,
    registerStep,
    unregisterStep,
    callOnStep,
    fetchJson(subpath, options) {
      return theoremContext.fetchJson(path.join("proofs", index.toString(), subpath), options)
    },
    fetchJsonForStep(stepPath, subpath, options) {
      return proofContext.fetchJson(path.join(stepPath.join("."), subpath), options)
    },
    fetchJsonForStepAndInsert(stepPath, subpath, options) {
      return proofContext.fetchJsonForStep(stepPath, subpath, options)
        .then(stepJson => theoremContext.insertSteps(index, stepJson))
    },
    fetchJsonForStepAndReplace(stepPath, subpath, options) {
      return proofContext.fetchJsonForStep(stepPath, subpath, options)
        .then(stepJson => theoremContext.replaceStep(index, stepJson))
        .then(([, newSteps]) => {
          const targetIndexes = getTargetStepIndexes(newSteps);
          if (targetIndexes.length === 1) {
            const newPath = [...stepPath.slice(0, stepPath.length - 1), stepPath[stepPath.length - 1] + targetIndexes[0]];
            proofContext.callOnStep(newPath, "startProving");
          }
        });
    },
    fetchJsonForStepAndInsertAndReplace(stepPath, subpath, options) {
      return proofContext.fetchJsonForStep(stepPath, subpath, options)
        .then(stepJson => theoremContext.insertAndReplaceSteps(index, stepJson))
        .then(([insertionPath, insertionSteps,]) => {
          const targetIndexes = getTargetStepIndexes(insertionSteps);
          if (targetIndexes.length === 1) {
            const newPath = [...insertionPath.slice(0, insertionPath.length - 1), insertionPath[insertionPath.length - 1] + targetIndexes[0]];
            proofContext.callOnStep(newPath, "startProving");
          }
        });
    },
    fetchJsonForStepAndInsertAndReplaceMultiple(stepPath, subpath, options) {
      return proofContext.fetchJsonForStep(stepPath, subpath, options)
        .then(stepJson => theoremContext.insertAndReplaceMultipleSteps(index, stepJson))
        .then(([insertionPath, insertionSteps, replacementPath, replacementSteps]) => {
          const replacementTargetIndexes = getTargetStepIndexes(replacementSteps);
          const insertionTargetIndexes = getTargetStepIndexes(insertionSteps);
          if (replacementTargetIndexes.length === 1) {
            const newPath = [...replacementPath.slice(0, replacementPath.length - 1), replacementPath[replacementPath.length - 1] + insertionSteps.length + replacementTargetIndexes[0]];
            proofContext.callOnStep(newPath, "startProving");
          } else if (replacementTargetIndexes.length === 0 && insertionTargetIndexes.length === 1) {
            const newPath = [...insertionPath.slice(0, insertionPath.length - 1), insertionPath[insertionPath.length - 1] + insertionTargetIndexes[0]];
            proofContext.callOnStep(newPath, "startProving");
          }
        });
    },
    fetchJsonAndInsertAndDelete(subpath, options) {
      return proofContext.fetchJson(subpath, options)
        .then(stepJson => theoremContext.insertAndDeleteSteps(index, stepJson));
    },
    fetchJsonForStepAndReplaceWithWrapping(stepPath, subpath, options) {
      return proofContext.fetchJsonForStep(stepPath, subpath, options)
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
              proofContext.callOnStep(path, "startProving");
            }
          }
        });
    },
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
    theoremContext.fetchJson("proofs", {method: "POST", body: index})
      .then(theoremContext.updateTheorem);
  };

  const deleteProof = () => {
    proofContext.fetchJson("", {method: "DELETE"})
      .then(theoremContext.updateTheorem);
  };

  return <ProofContext.Provider value={proofContext}>
    <hr/>
    {deleteable && <Button onClick={deleteProof} variant="danger" size="sm" className="float-right ml-1"><i className="fas fa-times"/></Button>}
    <Button onClick={duplicateProof} variant="primary" size="sm" className="float-right ml-1"><i className="fas fa-copy"/></Button>
    <h4>{title}</h4>
    <DndProvider backend={Backend}>
      <BoundVariableListContext.Provider value={[]}>
        <Steps.Container path={[]}>
          <Steps steps={steps}
                 path={[]}
                 propsForLastStep={{showConclusion: true}}
          />
        </Steps.Container>
      </BoundVariableListContext.Provider>
    </DndProvider>
  </ProofContext.Provider>;
}
