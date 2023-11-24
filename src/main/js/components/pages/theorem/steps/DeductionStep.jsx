import React from "react";
import {StepReference} from "../../../../models/Step";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

export function DeductionStep({step, path, additionalReferences, showConclusion}) {
  additionalReferences = additionalReferences || [];
  const reference = new StepReference(path);
  const referencesForAssumptionAndConclusion = showConclusion ? [] : [...additionalReferences, reference];
  return <Step.WithSubsteps path={path}>
    <Step.Antecedent>
      <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                           prefix="Assume"
                                           statement={step.assumption}
                                           path={path}
                                           suffix="a"
                                           additionalReferences={referencesForAssumptionAndConclusion}/>
    </Step.Antecedent>
    <Steps.Children steps={step.substeps}
                    path={path}
                    propsForLastStep={{additionalReferences: referencesForAssumptionAndConclusion}} />
    {step.provenStatement && showConclusion &&
      <ProofLine.SingleStatementWithPrefix prefix="So"
                                           statement={step.provenStatement}
                                           path={path}
                                           additionalReferences={additionalReferences}
                                           premiseReferences={[new StepReference(path, "a"), new StepReference([...path, step.substeps.length - 1])]} />}
  </Step.WithSubsteps>;
}
