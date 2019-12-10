import React from "react";
import {connect} from "react-redux";
import ProofContext from "../theorem/ProofContext";
import {FetchJsonForStepAndUpdate} from "../theorem/TheoremStore";
import ProofLine from "./ProofLine";
import {Steps} from "./Steps";
import {InlineTextEditor} from "../helpers/InlineTextEditor";

export const ScopedVariableStep = connect()(class ScopedVariableStep extends React.Component {
  static contextType = ProofContext;
  updateBoundVariable = (newName) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "boundVariable", {method: "PUT", body: newName}));
  };
  render() {
    let {step, path, boundVariableLists, additionalReferences} = this.props;
    let innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine path={path} boundVariableLists={boundVariableLists}>
        Take any
        {' '}
        <InlineTextEditor text={step.variableName} callback={this.updateBoundVariable}/>
        .
      </ProofLine>
      <Steps.Children steps={step.substeps}
                      path={path}
                      boundVariableLists={innerBoundVariableLists} />
      {step.provenStatement &&
        <ProofLine.SingleStatementWithPrefix prefix="So"
                                             statement={step.provenStatement}
                                             path={path}
                                             boundVariableLists={boundVariableLists}
                                             additionalReferences={additionalReferences}
                                             premiseReferences={[{stepPath: [...path, step.substeps.length - 1]}]} />}
    </>
  }
});
