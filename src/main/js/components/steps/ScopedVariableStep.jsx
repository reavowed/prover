import React from "react";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";
import {BoundVariableEditor} from "./BoundVariableEditor";

export class ScopedVariableStep extends React.Component {
  updateBoundVariable = (newName) => {
    return this.props.apiService
      .fetchJsonForStep(this.props.path, "boundVariable", {method: "PUT", body: newName})
      .then(this.props.apiService.updateTheorem);
  };
  render() {
    let {step, path, boundVariableLists, additionalReferences, apiService, highlighting} = this.props;
    let innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine path={path} apiService={apiService} boundVariableLists={boundVariableLists}>
        Take any
        {' '}
        <BoundVariableEditor name={step.variableName} callback={this.updateBoundVariable}/>
        .
      </ProofLine>
      <Steps.Children steps={step.substeps}
                      path={path}
                      boundVariableLists={innerBoundVariableLists}
                      apiService={apiService}
                      highlighting={highlighting}/>
      {step.provenStatement &&
        <ProofLine.SingleStatementWithPrefix prefix="So"
                                             statement={step.provenStatement}
                                             path={path}
                                             boundVariableLists={boundVariableLists}
                                             additionalReferences={additionalReferences}
                                             premiseReferences={[{stepPath: [...path, step.substeps.length - 1]}]}
                                             apiService={apiService}
                                             highlighting={highlighting}/>}
    </>
  }
}
