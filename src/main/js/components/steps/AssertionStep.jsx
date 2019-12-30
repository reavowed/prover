import React from "react";
import Button from "react-bootstrap/Button";
import {connect} from "react-redux";
import ProofContext from "../theorem/ProofContext";
import {FetchJsonForStepAndUpdate} from "../theorem/TheoremStore";
import {InferenceLink} from "./InferenceLink";
import ProofLine from "./ProofLine";

export const AssertionStepProofLine = connect()(class AssertionStepProofLine extends React.Component {
  static contextType = ProofContext;
  createTargets = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "createTargets", {method: "POST"}));
  };

  render() {
    let {step, path, children} = this.props;
    return <ProofLine premiseReferences={step.referencedLines}
                      path={path}
                      statement={step.statement}
                      buttons={<>
                        <InferenceLink inference={step.inference}/>
                        {!step.isComplete && <Button variant="success" size="sm" onClick={this.createTargets}>Create targets</Button>}
                      </>}
                      incomplete={!step.isComplete}>
      {children}
    </ProofLine>;
  }
});

export class AssertionStep extends React.Component {
  render() {
    const {step, path, additionalReferences} = this.props;
    return <AssertionStepProofLine {...this.props}>
      <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                  prefix="Then"
                                                  statement={step.statement}
                                                  path={path}
                                                  additionalReferences={additionalReferences} />
    </AssertionStepProofLine>
  }
}
