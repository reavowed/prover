import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import ProofContext from "../ProofContext";
import {InferenceLink} from "./components/InferenceLink";
import ProofLine from "./components/ProofLine";
import Step from "./Step";

export function AssertionStepProofLine({step, path, children}) {
  const context = useContext(ProofContext);
  const createTargets = () => {
    context.fetchJsonForStepAndUpdateTheorem(path, "createTargets", {method: "POST"});
  };
  return <ProofLine premiseReferences={step.referencedLines}
                    path={path}
                    statement={step.statement}
                    buttons={<>
                      <InferenceLink inference={step.inference}/>
                      {!step.isComplete && step.inference.isComplete && <Button variant="success" size="sm" onClick={createTargets}>Create targets</Button>}
                    </>}
                    incomplete={!step.isComplete}>
    {children}
  </ProofLine>;
}

export class AssertionStep extends React.Component {
  render() {
    const {step, path, additionalReferences} = this.props;
    return <Step.WithoutSubsteps>
      <AssertionStepProofLine {...this.props}>
        <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                    prefix="Then"
                                                    statement={step.statement}
                                                    path={path}
                                                    additionalReferences={additionalReferences} />
      </AssertionStepProofLine>
    </Step.WithoutSubsteps>
  }
}
