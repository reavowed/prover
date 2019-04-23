import React from "react";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";
import {BoundVariableEditor} from "./BoundVariableEditor";

export class NamingStep extends React.Component {
  updateBoundVariable = (newName) => {
    this.props.apiService
      .fetchJsonForStep(this.props.path, "boundVariable", {method: "PUT", body: newName})
      .then(this.props.apiService.updateTheorem);
  };
  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists,} = this.props;
    let reference = {stepPath: path};
    let referenceForAssumption = {stepPath: path, suffix: "a"};
    let referencesForLastStep = [...additionalReferences, reference];
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    const prefix = <>
        Let
        {' '}
        <BoundVariableEditor name={step.variableName} callback={this.updateBoundVariable}/>
        {' '}
        be such that
    </>;
    return <>
      <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                           prefix={prefix}
                                           statement={step.assumption}
                                           path={path}
                                           boundVariableLists={innerBoundVariableLists}
                                           additionalReferences={additionalReferences}
                                           premiseReferences={step.referencedLinesForExtraction}
                                           reference={referenceForAssumption}
                                           buttons={<InferenceLink inference={step.inference}/>}
                                           apiService={apiService}
                                           highlighting={highlighting}/>
      <Steps steps={step.substeps}
             path={path}
             boundVariableLists={innerBoundVariableLists}
             referencesForLastStep={referencesForLastStep}
             apiService={apiService}
             highlighting={highlighting} />
    </>;
  }
}
