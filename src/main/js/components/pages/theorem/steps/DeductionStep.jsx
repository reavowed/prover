import React from "react";
import {StepReference} from "../../../../models/Step";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

export function DeductionStep({step, path, additionalReferences}) {
  additionalReferences = additionalReferences || [];
  const reference = new StepReference(path);
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
                    propsForLastStep={{additionalReferences: [...additionalReferences, reference]}} />
  </Step.WithSubsteps>;
}
