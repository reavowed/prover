import React from "react";
import Button from "react-bootstrap/Button";
import {HighlightableExpression} from "../ExpressionComponent";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";
import {ClickableText} from "./ClickableText";
import {BoundVariableModal} from "../Modals";

export class AssertionStepProofLine extends React.Component {
  createTargets = () => {
    this.props.theoremContext.fetchJsonForStep(this.props.path, "createTargets", {
      method: "POST"
    }).then(this.props.theoremContext.updateTheorem);
  };

  render() {
    let {step, path, theoremContext, children, boundVariableLists} = this.props;
    return <ProofLine premiseReferences={step.referencedLines}
                      path={path}
                      statement={step.statement}
                      boundVariableLists={boundVariableLists}
                      buttons={<>
                        <InferenceLink inference={step.inference}/>
                        {step.isIncomplete && <Button variant="success" size="sm" onClick={this.createTargets}>Create targets</Button>}
                      </>}
                      theoremContext={theoremContext}
                      incomplete={step.isIncomplete}>
      {children}
    </ProofLine>;
  }
}

export class AssertionStep extends React.Component {
  render() {
    const {step, path, boundVariableLists, theoremContext} = this.props;
    return <AssertionStepProofLine {...this.props}>
      <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                  prefix="Then"
                                                  statement={step.statement}
                                                  path={path}
                                                  boundVariableLists={boundVariableLists}
                                                  theoremContext={theoremContext} />
    </AssertionStepProofLine>
  }
}
