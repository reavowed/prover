import React from "react";
import {HighlightableExpression} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";
import {BoundVariableModal} from "../Modals";
import {ClickableText} from "./ClickableText";

export class DeductionStep extends React.Component {
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
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists} = this.props;
    let reference = {stepPath: path};
    let referenceForAssumption = {stepPath: path, suffix: "a"};
    let referencesForLastStep = [...additionalReferences, reference];
    const wrapEditableBoundVariable = (boundVariableContent, boundVariableName, boundVariableIndex, boundVariablePath) =>
      <ClickableText
        onClick={() => this.showBoundVariableModal(boundVariableName, boundVariableIndex, boundVariablePath)}>
        {boundVariableContent}
      </ClickableText>;
    return <>
      <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                           prefix="Assume"
                                           statement={step.assumption}
                                           path={path}
                                           boundVariableLists={boundVariableLists}
                                           additionalReferences={[...additionalReferences, referenceForAssumption]}
                                           apiService={apiService}
                                           highlighting={highlighting}/>
      <Steps.Children steps={step.substeps}
                      path={path}
                      boundVariableLists={boundVariableLists}
                      referencesForLastStep={referencesForLastStep}
                      apiService={apiService}
                      highlighting={highlighting} />
      <BoundVariableModal show={this.state.boundVariableModalCallback != null}
                          onHide={this.hideBoundVariableModal}
                          title="Rename bound variable"
                          value={this.state.boundVariableName}
                          onChange={e => this.setState({boundVariableName: e.target.value})}
                          onSave={this.state.boundVariableModalCallback}/>
    </>;
  }
}
