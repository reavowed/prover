import React from "react";
import Button from "react-bootstrap/Button";
import {HighlightableExpression} from "../ExpressionComponent";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";
import {ClickableText} from "./ClickableText";
import {Steps} from "./Steps";
import {BoundVariableModal} from "../Modals";

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
                      statement={step.statement}
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

export class AssertionStep extends React.Component {
  constructor(...args) {
    super(...args)
    this.state = {
      boundVariableModalCallback: null,
      boundVariableName: ""
    }
  }
  showBoundVariableModal = (boundVariableName, boundVariableIndex, boundVariablePath) => {
    this.setState({
      boundVariableName,
      boundVariableModalCallback: () => this.updateBoundVariable(boundVariableIndex, boundVariablePath)
    })
  };
  hideBoundVariableModal = () => {
    this.setState({
      boundVariableModalCallback: null
    })
  };
  updateBoundVariable = (boundVariableIndex, boundVariablePath) => {
    this.props.apiService.fetchJsonForStep(this.props.path, `boundVariables/${boundVariablePath.join(".")}/${boundVariableIndex}/`, {
      method: "PUT",
      body: this.state.boundVariableName
    })
      .then(this.props.apiService.updateTheorem)
      .then(this.hideBoundVariableModal);
  };
  render() {
    const {step, boundVariableLists, additionalReferences, highlighting, path} = this.props;
    const wrapEditableBoundVariable = (boundVariableContent, boundVariableName, boundVariableIndex, boundVariablePath) =>
      <ClickableText
        onClick={() => this.showBoundVariableModal(boundVariableName, boundVariableIndex, boundVariablePath)}>
        {boundVariableContent}
      </ClickableText>;
    return <AssertionStepProofLine {...this.props}>
      Then
      {' '}
      <HighlightableExpression statement={step.statement}
                               boundVariableLists={boundVariableLists}
                               wrapBoundVariable={wrapEditableBoundVariable}
                               references={[...additionalReferences, {stepPath: path}]}
                               highlighting={highlighting}/>.
      <BoundVariableModal show={this.state.boundVariableModalCallback != null}
                          onHide={this.hideBoundVariableModal}
                          title="Rename bound variable"
                          value={this.state.boundVariableName}
                          onChange={e => this.setState({boundVariableName: e.target.value})}
                          onSave={this.state.boundVariableModalCallback}/>
    </AssertionStepProofLine>
  }
};
