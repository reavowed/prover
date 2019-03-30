import React from "react";
import {formatHtml, HighlightableExpression} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class NamingStep extends React.Component {
  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists, onPopover} = this.props;
    let reference = path.join(".");
    let assumptionReference = reference + "a";
    let referencesForLastStep = [...additionalReferences, reference];
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine premiseReferences={step.inference.referencedLines}
                 reference={assumptionReference}
                 path={path}
                 apiService={apiService}
                 highlighting={highlighting}>
        Let {formatHtml(step.variableName)} be such that
        {' '}
        <HighlightableExpression statement={step.assumption}
                                 boundVariableLists={innerBoundVariableLists}
                                 reference={assumptionReference}
                                 highlighting={highlighting}/>.
      </ProofLine>
      <Steps steps={step.substeps}
             path={path}
             onPopover={onPopover}
             boundVariableLists={innerBoundVariableLists}
             referencesForLastStep={referencesForLastStep}
             apiService={apiService}
             highlighting={highlighting} />
    </>;
  }
}
