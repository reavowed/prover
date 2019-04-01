import React from "react";
import {HighlightableExpression} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class DeductionStep extends React.Component {
  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists} = this.props;
    let reference = {stepPath: path};
    let referenceForAssumption = {stepPath: path, suffix: "a"};
    let referencesForLastStep = [...additionalReferences, reference];
    return <>
      <ProofLine path={path} apiService={apiService}>
        Assume
        {' '}
        <HighlightableExpression expression={step.assumption}
                                 boundVariableLists={boundVariableLists}
                                 references={[...additionalReferences, reference, referenceForAssumption]}
                                 apiService={apiService}
                                 highlighting={highlighting}/>.
      </ProofLine>
      <Steps.Children steps={step.substeps}
                      path={path}
                      boundVariableLists={boundVariableLists}
                      referencesForLastStep={referencesForLastStep}
                      apiService={apiService}
                      highlighting={highlighting} />
    </>;
  }
}
