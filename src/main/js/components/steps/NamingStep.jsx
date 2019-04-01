import React from "react";
import {formatHtml, HighlightableExpression} from "../ExpressionComponent";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class NamingStep extends React.Component {
  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists,} = this.props;
    let reference = {stepPath: path};
    let referenceForAssumption = {stepPath: path, suffix: "a"};
    let referencesForLastStep = [...additionalReferences, reference];
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine premiseReferences={step.referencedLines}
                 reference={referenceForAssumption}
                 buttons={<InferenceLink inference={step.inference}/>}
                 path={path}
                 apiService={apiService}
                 highlighting={highlighting}>
        Let {formatHtml(step.variableName)} be such that
        {' '}
        <HighlightableExpression statement={step.assumption}
                                 boundVariableLists={innerBoundVariableLists}
                                 reference={referenceForAssumption}
                                 highlighting={highlighting}/>.
      </ProofLine>
      <Steps steps={step.substeps}
             path={path}
             boundVariableLists={innerBoundVariableLists}
             referencesForLastStep={referencesForLastStep}
             apiService={apiService}
             highlighting={highlighting} />
    </>;
  }
}
