import React from "react";
import {ExpressionComponent, HighlightableExpression} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class ScopedVariableStep extends React.Component {
  render() {
    let {step, path, boundVariableLists, additionalReferences, apiService, highlighting} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    let innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine>Take any <ExpressionComponent expression={{textForHtml: () => step.variableName}}/>.</ProofLine>
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
    </>
  }
}
