import React, {PropsWithChildren, useContext} from "react";
import Button from "react-bootstrap/Button";
import ProofContext from "../ProofContext";
import {InferenceLink} from "./components/InferenceLink";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {StepProps} from "./Steps";
import {AssertionStep as AssertionStepModel} from "../../../../models/Step";

export function AssertionStepProofLine({step, children}: PropsWithChildren<StepProps<AssertionStepModel>>) {
  const context = useContext(ProofContext)!;
  const createTargets = () => context.fetchJsonForStepAndInsert(step.path, "createTargets", {method: "POST"});
  return <ProofLine premiseReferences={step.referencedLines}
                    path={step.path}
                    buttons={<>
                      <InferenceLink inference={step.inference}/>
                      {!step.isComplete && step.inference.isComplete && <Button variant="success" size="sm" onClick={createTargets}>Create targets</Button>}
                    </>}
                    incomplete={!step.isComplete}>
    {children}
  </ProofLine>;
}

export function AssertionStep({step, additionalReferences}: StepProps<AssertionStepModel>) {
  return <Step.WithoutSubsteps>
    <AssertionStepProofLine step={step}>
      <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                  prefix="Then"
                                                  statement={step.statement}
                                                  path={step.path}
                                                  additionalReferences={additionalReferences} />
    </AssertionStepProofLine>
  </Step.WithoutSubsteps>
}
