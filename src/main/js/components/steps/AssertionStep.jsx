import React from "react";
import Button from "react-bootstrap/Button";
import {HighlightableExpression} from "../ExpressionComponent";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";

export class AssertionStepProofLine extends React.Component {
  createTargets = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "createTargets", {
      method: "POST"
    }).then(this.props.apiService.updateTheorem);
  };

  render() {
    let {step, path, apiService, highlighting, children} = this.props;
    return <ProofLine premiseReferences={step.referencedLines}
                      path={path}
                      buttons={<>
                        <InferenceLink inference={step.inference}/>
                        {step.isIncomplete && <Button variant="success" size="sm" onClick={this.createTargets}>Create targets</Button>}
                      </>}
                      highlighting={highlighting}
                      apiService={apiService}
                      incomplete={step.isIncomplete}>
      {children}
    </ProofLine>;
  }
}

export const AssertionStep = (props) => {
  const {step, boundVariableLists, additionalReferences, highlighting, path} = props;
  return <AssertionStepProofLine {...props}>
    Then
    {' '}
    <HighlightableExpression statement={step.statement}
                             boundVariableLists={boundVariableLists}
                             references={[...additionalReferences, {stepPath: path}]}
                             highlighting={highlighting}/>.</AssertionStepProofLine>
};
