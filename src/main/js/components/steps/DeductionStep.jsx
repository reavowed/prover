import React from "react";
import {connect} from "react-redux";
import {StepReference} from "../../models/Step";
import ProofContext from "../theorem/ProofContext";
import {FetchJsonForStepAndUpdate} from "../theorem/TheoremStore";
import ProofLine from "./ProofLine";
import {Steps} from "./Steps";
import {BoundVariableModal} from "../Modals";

export const DeductionStep = connect()(class DeductionStep extends React.Component {
  static contextType = ProofContext;
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
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, `boundVariables/${boundVariablePath.join(".")}/${boundVariableIndex}/`, {
      method: "PUT",
      body: this.state.boundVariableName
    })).then(this.hideBoundVariableModal);
  };
  render() {
    let {step, path, additionalReferences} = this.props;
    let reference = new StepReference(path);
    let referencesForLastStep = [...additionalReferences, reference];
    return <>
      <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                           prefix="Assume"
                                           statement={step.assumption}
                                           path={path}
                                           suffix="a"
                                           additionalReferences={[...additionalReferences, reference]}/>
      <Steps.Children steps={step.substeps}
                      path={path}
                      referencesForLastStep={referencesForLastStep} />
      <BoundVariableModal show={this.state.boundVariableModalCallback != null}
                          onHide={this.hideBoundVariableModal}
                          title="Rename bound variable"
                          value={this.state.boundVariableName}
                          onChange={e => this.setState({boundVariableName: e.target.value})}
                          onSave={this.state.boundVariableModalCallback}/>
    </>;
  }
});
