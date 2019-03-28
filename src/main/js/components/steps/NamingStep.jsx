import React from "react";
import {formatHtml, HighlightableExpression} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class NamingStep extends React.Component {
  render() {
    const {step, path, boundVariableLists, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let assumptionReference = reference + "a";
    let referencesForLastStep = [...additionalReferences, reference];
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine premiseReferences={step.inference.referencedLines} reference={assumptionReference} {...otherProps}>
        Let {formatHtml(step.variableName)} be such that
        {' '}
        <HighlightableExpression statement={step.assumption}
                                 boundVariableLists={innerBoundVariableLists}
                                 reference={assumptionReference}
                                 {...otherProps}/>.
      </ProofLine>
      <Steps steps={step.substeps} path={path} boundVariableLists={innerBoundVariableLists} referencesForLastStep={referencesForLastStep} {...otherProps} />
    </>;
  }
}
