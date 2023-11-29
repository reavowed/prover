import update from "immutability-helper";
import _ from "lodash";
import React, {useContext, useRef} from "react";
import {DefinedExpression, Expression} from "../../../../models/Expression";
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
import {ElidedStepProofLine} from "./ElidedStep";
import Step from "./Step";
import {ElidableVariableDescription, matchElidableVariableDescription} from "./stepDisplayFunctions";
import Steps, {StepProps} from "./Steps";
import {SimpleReactNode} from "../../../../utils";
import ProofLine from "./components/ProofLine";
import {InferenceLink} from "./components/InferenceLink";

type NamingStepProps = StepProps<NamingStepModel> & {
  assertionStep?: AssertionStep | ElidedStep
}
type CollapsedNamingStepDetails = {
  step: NamingStepModel
  assumption: Expression
  variableDescription?: ElidableVariableDescription
}

export default function NamingStep({step: outerNamingStep, assertionStep, additionalReferences}: NamingStepProps) {
  additionalReferences = additionalReferences || [];
  const context = useContext(ProofContext)!;
  const displaySettings = useContext(DisplaySettingsContext);
  const proofLineRef = useRef(null);
  const updateBoundVariable = (namingStepPath: number[]) => (newName: string) => {
    return context.fetchJsonForStepAndReplace(namingStepPath, "boundVariable", {method: "PUT", body: newName});
  };

  const namingSteps: CollapsedNamingStepDetails[] = [];
  let elidableVariableDescription;
  function addNamingStep(namingStep: NamingStepModel) {
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
    _.some(currentNamingStep.substeps[0].premiseReferences, r => (r instanceof StepReference) && _.isEqual(r.stepPath, currentNamingStep.path) && r.suffix === "a"))
  {
    currentNamingStep = currentNamingStep.substeps[0];
    addNamingStep(currentNamingStep);
  }

  const outerNamingStepReference = new StepReference(outerNamingStep.path);
  const innermostNamingStep = namingSteps[namingSteps.length - 1];
  const innermostAssumptionReference = new StepReference(innermostNamingStep.step.path, "a");

  function getNamingStepVariableDescriptions(namingSteps: CollapsedNamingStepDetails[], boundVariableLists: string[][]): SimpleReactNode[] {
    return _.reduce<CollapsedNamingStepDetails, [SimpleReactNode[], string[][]]>(
      namingSteps,
      ([stepsSoFar, boundVariableLists], {step, variableDescription}) => {
        let stepElement;
        if (variableDescription) {
          const assumptionReference = new StepReference(step.path, "a");
          const patchedExpression = new DefinedExpression(variableDescription.definition, [step.variableName], variableDescription.otherComponents);
          const wrapBoundVariable = (name: string, index: number, boundVariablePath: number[]) => {
            const callback = (_.isEqual(boundVariablePath, [0]) && index === 0) ?
              updateBoundVariable(step.path) :
              (newName: string) => context.fetchJsonForStepAndReplace(step.path, `boundVariables/${update(boundVariablePath, {$splice: [[0, 1, boundVariablePath[0] + 1]]}).join(".")}/${index}/`, {method: "PUT", body: newName});
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

  function wrapEditableBoundVariableInAssumption(name: string, index: number, boundVariablePath: number[]) {
    const callback = (newName: string) => {
      return context.fetchJsonForStepAndReplace(innermostNamingStep.step.path, `boundVariables/${boundVariablePath.join(".")}/${index}/`, {
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
    <ElidedStepProofLine step={assertionStep} additionalReferences={[new StepReference(outerNamingStep.path)]}>
      {proofLineContent}
    </ElidedStepProofLine> :
    (assertionStep instanceof AssertionStep) ?
    <AssertionStepProofLine step={assertionStep} additionalReferences={[new StepReference(outerNamingStep.path)]}>
      {proofLineContent}
    </AssertionStepProofLine> :
    <ProofLine path={outerNamingStep.path}
               suffix="a"
               premiseReferences={innermostNamingStep.step.premiseReferences}
               buttons={<span className="mr-2"><InferenceLink inference={innermostNamingStep.step.inference}/></span>}
               ref={proofLineRef}>
      {proofLineContent}
    </ProofLine>;

  return <Step.WithSubsteps path={innermostNamingStep.step.path}>
      {proofLine}
      <AddBoundVariableLists variableLists={namingSteps.map(({step}) => [step.variableName])}>
        <Steps steps={innermostNamingStep.step.substeps}
               path={innermostNamingStep.step.path}
               propsForLastStep={{additionalReferences: [...additionalReferences, outerNamingStepReference]}} />
      </AddBoundVariableLists>
    </Step.WithSubsteps>;
}
