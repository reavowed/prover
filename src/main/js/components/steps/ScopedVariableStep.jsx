import React from "react";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";
import {InlineTextEditor} from "../helpers/InlineTextEditor";

export class ScopedVariableStep extends React.Component {
  updateBoundVariable = (newName) => {
    return this.props.theoremContext
      .fetchJsonForStep(this.props.path, "boundVariable", {method: "PUT", body: newName})
      .then(this.props.theoremContext.updateTheorem);
  };
  render() {
    let {step, path, boundVariableLists, additionalReferences, theoremContext} = this.props;
    let innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine path={path} theoremContext={theoremContext} boundVariableLists={boundVariableLists}>
        Take any
        {' '}
        <InlineTextEditor text={step.variableName} callback={this.updateBoundVariable}/>
        .
      </ProofLine>
      <Steps.Children steps={step.substeps}
                      path={path}
                      boundVariableLists={innerBoundVariableLists}
                      theoremContext={theoremContext} />
      {step.provenStatement &&
        <ProofLine.SingleStatementWithPrefix prefix="So"
                                             statement={step.provenStatement}
                                             path={path}
                                             boundVariableLists={boundVariableLists}
                                             additionalReferences={additionalReferences}
                                             premiseReferences={[{stepPath: [...path, step.substeps.length - 1]}]}
                                             theoremContext={theoremContext} />}
    </>
  }
}
