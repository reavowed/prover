import React from "react";
import {VariableOrConstant} from "../../models/Expression";
import {ExpressionComponent, HighlightableExpression} from "../ExpressionComponent";
import {BoundVariableModal} from "../Modals";
import {ClickableText} from "./ClickableText";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class ScopedVariableStep extends React.Component {
  constructor(...args) {
    super(...args)
    this.state = {
      showBoundVariableModal: false,
      boundVariableName: ""
    }
  }
  updateBoundVariable = () => {
    this.props.apiService
      .fetchJsonForStep(this.props.path, "boundVariable", {method: "PUT", body: this.state.boundVariableName})
      .then(this.props.apiService.updateTheorem);
  };
  render() {
    let {step, path, boundVariableLists, additionalReferences, apiService, highlighting} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    let innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine>
        Take any
        {' '}
        <ClickableText onClick={() => this.setState({showBoundVariableModal: true, boundVariableName: step.variableName})}>
          <ExpressionComponent expression={new VariableOrConstant(step.variableName)}/>
        </ClickableText>
        .
      </ProofLine>
      <Steps.Children steps={step.substeps}
                      path={path}
                      boundVariableLists={innerBoundVariableLists}
                      apiService={apiService}
                      highlighting={highlighting}/>
      {step.provenStatement &&
        <ProofLine highlighting={highlighting}
                   apiService={apiService}
                   premiseReferences={[{lineReference: [...path, step.substeps.length - 1].join("."), internalPath: []}]}
                   path={path}>
          So <HighlightableExpression expression={step.provenStatement}
                                      boundVariableLists={boundVariableLists}
                                      references={referencesForLastStep}
                                      apiService={apiService}
                                      highlighting={highlighting}/>.
        </ProofLine>}
      <BoundVariableModal show={this.state.showBoundVariableModal}
                          onHide={() => this.setState({showBoundVariableModal: false})}
                          title="Introduce bound variable"
                          label="Bound variable name"
                          value={this.state.boundVariableName}
                          onChange={e => this.setState({boundVariableName: e.target.value})}
                          onSave={this.updateBoundVariable}/>
    </>
  }
}
