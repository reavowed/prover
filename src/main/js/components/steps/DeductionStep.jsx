import React from "react";
import {HighlightableStatement} from "../Expression";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class DeductionStep extends React.Component {
  render() {
    let {step, path, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    return <>
      <ProofLine>Assume <HighlightableStatement statement={step.assumption} boundVariableLists={this.props.boundVariableLists} references={[...additionalReferences, reference, reference + "a"]} {...otherProps}/>.</ProofLine>
      <Steps.Children steps={step.substeps} path={path} referencesForLastStep={referencesForLastStep} {...otherProps} />
    </>;
  }
}
