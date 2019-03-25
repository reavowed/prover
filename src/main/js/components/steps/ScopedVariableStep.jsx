import React from "react";
import {Steps} from "./Steps";

export class ScopedVariableStep extends React.Component {
  render() {
    let {step, path, boundVariableLists, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    return <Steps steps={step.substeps} path={path} boundVariableLists={[[step.variableName], ...boundVariableLists]} referencesForLastStep={referencesForLastStep} {...otherProps} />
  }
}