import update from "immutability-helper";
import _ from "lodash";
import React, {useContext, useRef} from "react";
import {DefinedExpression} from "../../../../models/Expression";
import {AssertionStep, ElidedStep, NamingStep as NamingStepModel} from "../../../../models/Step";
import {StepReference} from "../../../definitions/Reference";
import {DisplaySettingsContext} from "../../../DisplaySettings";
import AddBoundVariableLists from "../../../expressions/boundVariables/AddBoundVariableLists";
import BoundVariableListContext from "../../../expressions/boundVariables/BoundVariableListContext";
import {HighlightableExpression} from "../../../expressions/ExpressionComponent";
import {InlineTextEditor} from "../../../helpers/InlineTextEditor";
import {joinAsList} from "../../../helpers/reactFunctions";
import ProofContext from "../ProofContext";
import {AssertionStepProofLine} from "./AssertionStep";
import {InferenceLink} from "./components/InferenceLink";
import ProofLine from "./components/ProofLine";
import {ElidedStepProofLine} from "./ElidedStep";
import Step from "./Step";
import {matchElidableVariableDescription} from "./stepDisplayFunctions";
import {Steps} from "./Steps";

export default function NamingStep({step: outerNamingStep, assertionStep, additionalReferences}) {
  additionalReferences = additionalReferences || [];
  const context = useContext(ProofContext);
  const displaySettings = useContext(DisplaySettingsContext);
  const proofLineRef = useRef(null);
  const updateBoundVariable = (namingStepPath) => (newName) => {
    return context.fetchJsonForStepAndReplace(namingStepPath, "boundVariable", {method: "PUT", body: newName});
  };

  const namingSteps = [];
  let elidableVariableDescription;
  function addNamingStep(namingStep) {
    if (namingStep.assumption instanceof DefinedExpression &&
      _.includes(namingStep.assumption.definition.attributes, "conjunction") &&
      (elidableVariableDescription = matchElidableVariableDescription(namingStep.assumption.components[0]))) {
      namingSteps.push({
        step: namingStep,
        variableDescription: elidableVariableDescription,
        assumption: namingStep.assumption.components[1]
      });
    } else {
      namingSteps.push({
        step: namingStep,
        assumption: namingStep.assumption
      });
    }
  }
  addNamingStep(outerNamingStep);
  let currentNamingStep = outerNamingStep;
  while (!displaySettings.disableAssumptionCollapse &&
    currentNamingStep.substeps.length === 1 &&
    currentNamingStep.substeps[0] instanceof NamingStepModel &&
    _.some(currentNamingStep.substeps[0].premiseReferences, r => _.isEqual(r.stepPath, currentNamingStep.path) && r.suffix === "a"))
  {
    currentNamingStep = currentNamingStep.substeps[0];
    addNamingStep(currentNamingStep);
  }

  const outerNamingStepReference = new StepReference(outerNamingStep.path);
  const innermostNamingStep = namingSteps[namingSteps.length - 1];
  const innermostNamingPath = innermostNamingStep.step.path;
  const innermostAssumptionReference = new StepReference(innermostNamingPath, "a");

  function getNamingStepVariableDescriptions(namingSteps, boundVariableLists) {
    return _.reduce(
      namingSteps,
      ([stepsSoFar, boundVariableLists], {step, variableDescription}) => {
        let stepElement;
        if (variableDescription) {
          const assumptionReference = new StepReference(step.path, "a");
          const patchedExpression = new DefinedExpression(variableDescription.definition, [step.variableName], variableDescription.otherComponents);
          const wrapBoundVariable = (name, index, boundVariablePath) => {
            const callback = (_.isEqual(boundVariablePath, [0]) && index === 0) ?
              updateBoundVariable(step.path) :
              (newName) => context.fetchJsonForStepAndReplace(step.path, `boundVariables/${update(boundVariablePath, {$splice: [[0, 1, boundVariablePath[0] + 1]]}).join(".")}/${index}/`, {method: "PUT", body: newName});
            return <InlineTextEditor text={name} callback={callback} />;
          };
          stepElement = <HighlightableExpression expression={patchedExpression} references={[assumptionReference]} additionalConclusionReferences={[innermostAssumptionReference]} wrapBoundVariable={wrapBoundVariable} path={[0]} />
        } else {
          stepElement = <InlineTextEditor text={step.variableName} callback={updateBoundVariable(step.path)}/>;
        }
        const newBoundVariableLists = [...boundVariableLists, [step.variableName]];
        return [[...stepsSoFar, <BoundVariableListContext.Provider value={boundVariableLists}>{stepElement}</BoundVariableListContext.Provider>], newBoundVariableLists];
      },
      [[], boundVariableLists]
    )[0];
  }

  function wrapEditableBoundVariableInAssumption(name, index, boundVariablePath) {
    const callback = (newName) => {
      return context.fetchJsonForStepAndReplace(innermostNamingPath, `boundVariables/${boundVariablePath.join(".")}/${index}/`, {
        method: "PUT",
        body: newName
      });
    };
    return <InlineTextEditor text={name} callback={callback} />;
  }

  const proofLineContent = <>
    Let
    {' '}
    <BoundVariableListContext.Consumer>{boundVariableLists =>
      joinAsList(getNamingStepVariableDescriptions(namingSteps, boundVariableLists))
    }</BoundVariableListContext.Consumer>
    {' '}
    be such that
    {' '}
    <AddBoundVariableLists variableLists={namingSteps.map(({step}) => [step.variableName])}>
      <HighlightableExpression expression={innermostNamingStep.assumption}
                               references={[innermostAssumptionReference]}
                               path={innermostNamingStep.variableDescription && [1]}
                               wrapBoundVariable={wrapEditableBoundVariableInAssumption}/>
    </AddBoundVariableLists>
    {'.'}
  </>;

  const proofLine = (assertionStep instanceof ElidedStep) ?
    <ElidedStepProofLine step={assertionStep} path={outerNamingStep.path}>
      {proofLineContent}
    </ElidedStepProofLine> :
    assertionStep instanceof AssertionStep ?
    <AssertionStepProofLine step={assertionStep} path={outerNamingStep.path}>
      {proofLineContent}
    </AssertionStepProofLine> :
    <ProofLine path={outerNamingStep.path}
               suffix="a"
               premiseReferences={[...innermostNamingStep.step.premiseReferences, ...(assertionStep ? _.filter(assertionStep.referencedLines, r => !_.startsWith(r.stepPath, outerNamingStep.path)) : [])]}
               buttons={<span className="mr-2"><InferenceLink inference={innermostNamingStep.step.inference}/></span>}
               ref={proofLineRef}>
      {proofLineContent}
    </ProofLine>;

  return <Step.WithSubsteps path={innermostNamingPath}>
      {proofLine}
      <AddBoundVariableLists variableLists={namingSteps.map(({step}) => [step.variableName])}>
        <Steps steps={innermostNamingStep.step.substeps}
               path={innermostNamingPath}
               propsForLastStep={{additionalReferences: [...additionalReferences, outerNamingStepReference]}} />
      </AddBoundVariableLists>
    </Step.WithSubsteps>;
}
