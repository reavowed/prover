import update from 'immutability-helper';
import React, {useContext} from "react";
import {DefinedExpression} from "../../../../models/Expression";

import {StepReference} from "../../../definitions/Reference";
import {HighlightableExpression} from "../../../expressions/ExpressionComponent";
import AddBoundVariableList from "../../../expressions/boundVariables/AddBoundVariableList";
import {InlineTextEditor} from "../../../helpers/InlineTextEditor";
import ProofContext from "../ProofContext";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import Steps, {StepProps} from "./Steps";
import {
  DeductionStep as DeductionStepModel,
  GeneralizationStep as GeneralizationStepModel
} from "../../../../models/Step";
import {ElidableVariableDescription} from "./stepDisplayFunctions";

type GeneralizedDeductionStepProps = StepProps<GeneralizationStepModel> & {
  variableDescription: ElidableVariableDescription
};

export function GeneralizedDeductionStep({step, additionalReferences = [], variableDescription, showConclusion}: GeneralizedDeductionStepProps) {
  const context = useContext(ProofContext)!;
  const substep = step.substeps[0] as DeductionStepModel;

  const reference = new StepReference(step.path);
  const assumptionReference = new StepReference(substep.path, "a");

  const wrapBoundVariable = (name: string, index: number, boundVariablePath: number[]) => {
    const callback = (boundVariablePath.length === 0 && index === 0) ?
      (newName: string) => context.fetchJsonForStepAndReplace(step.path, "boundVariable", {method: "PUT", body: newName}) :
      (newName: string) => context.fetchJsonForStepAndReplace(substep.path, `boundVariables/${update(boundVariablePath, {$splice: [[0, 1, boundVariablePath[0] + 1]]}).join(".")}/${index}/`, {method: "PUT", body: newName});
    return <InlineTextEditor text={name} callback={callback} />;
  };

  const patchedExpression = new DefinedExpression(variableDescription.definition, [step.variableName], variableDescription.otherComponents);
  const referencesForLastStep = showConclusion ? [] : [...additionalReferences, reference];

  return <Step.WithSubsteps path={step.path}>
    <Step.Antecedent>
        <ProofLine path={step.path}>
          Take any <HighlightableExpression expression={patchedExpression} references={[assumptionReference]} additionalPremiseReferences={referencesForLastStep} wrapBoundVariable={wrapBoundVariable} expressionToCopy={substep.assumption}/>.
        </ProofLine>
    </Step.Antecedent>
    <AddBoundVariableList variables={[step.variableName]}>
      <Steps.Children steps={substep.substeps} path={substep.path} propsForLastStep={{additionalReferences: referencesForLastStep}} />
    </AddBoundVariableList>
    {step.provenStatement && showConclusion &&
      <ProofLine.SingleStatementWithPrefix prefix="So"
                                           statement={step.provenStatement}
                                           path={step.path}
                                           additionalReferences={additionalReferences}
                                           premiseReferences={[assumptionReference, new StepReference([...substep.path, substep.substeps.length - 1])]} />}
  </Step.WithSubsteps>;
}
