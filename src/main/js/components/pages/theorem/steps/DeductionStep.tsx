import React, {PropsWithChildren} from "react";

import {StepReference} from "../../../definitions/Reference";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import Steps, {StepProps} from "./Steps";
import {DeductionStep as DeductionStepModel} from "../../../../models/Step";

export function DeductionStep({step, additionalReferences, showConclusion}: PropsWithChildren<StepProps<DeductionStepModel>>) {
  additionalReferences = additionalReferences || [];
  const reference = new StepReference(step.path);
  const referencesForAssumptionAndConclusion = showConclusion ? [] : [...additionalReferences, reference];
  return <Step.WithSubsteps path={step.path}>
    <Step.Antecedent>
      <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                           prefix="Assume"
                                           statement={step.assumption}
                                           path={step.path}
                                           suffix="a"
                                           additionalReferences={referencesForAssumptionAndConclusion}/>
    </Step.Antecedent>
    <Steps.Children steps={step.substeps}
                    path={step.path}
                    propsForLastStep={{additionalReferences: referencesForAssumptionAndConclusion}} />
    {step.provenStatement && showConclusion &&
      <ProofLine.SingleStatementWithPrefix prefix="So"
                                           statement={step.provenStatement}
                                           path={step.path}
                                           additionalReferences={additionalReferences}
                                           premiseReferences={[new StepReference(step.path, "a"), new StepReference([...step.path, step.substeps.length - 1])]} />}
  </Step.WithSubsteps>;
}
