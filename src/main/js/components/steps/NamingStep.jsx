import React from "react";
import {formatHtml} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class NamingStep extends React.Component {
  render() {
    const {step, path, boundVariableLists, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine premiseReferences={step.inference.referencedLines} {...otherProps}>
        Let {formatHtml(step.variableName)} be such that <ProofLine.Statement statement={step.assumption} boundVariableLists={innerBoundVariableLists} references={[...additionalReferences, reference, reference + "a"]} {...otherProps}/>.
      </ProofLine>
      <Steps steps={step.substeps} path={path} boundVariableLists={innerBoundVariableLists} referencesForLastStep={referencesForLastStep} {...otherProps} />
    </>;
  }
}
