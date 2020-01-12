import React from "react";
import {StepReference} from "../../../../models/Step";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

export function DeductionStep({step, path, additionalReferences}) {
  let reference = new StepReference(path);
  let referencesForLastStep = [...additionalReferences, reference];
  return <Step.WithSubsteps path={path}>
    <Step.Antecedent>
      <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                           prefix="Assume"
                                           statement={step.assumption}
                                           path={path}
                                           suffix="a"
                                           additionalReferences={[...additionalReferences, reference]}/>
    </Step.Antecedent>
    <Steps.Children steps={step.substeps}
                    path={path}
                    referencesForLastStep={referencesForLastStep} />
  </Step.WithSubsteps>;
}
