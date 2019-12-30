import React from "react";
import {connect} from "react-redux";
import {StepReference} from "../../models/Step";
import ProofContext from "../theorem/ProofContext";
import {FetchJsonForStepAndUpdate} from "../theorem/TheoremStore";
import ProofLine from "./ProofLine";
import {Steps} from "./Steps";
import {InlineTextEditor} from "../helpers/InlineTextEditor";
import BoundVariableLists from "./BoundVariableLists";

export const ScopedVariableStep = connect()(class ScopedVariableStep extends React.Component {
  static contextType = ProofContext;
  updateBoundVariable = (newName) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "boundVariable", {method: "PUT", body: newName}));
  };
  render() {
    let {step, path, additionalReferences} = this.props;
    return <>
      <ProofLine path={path}>
        Take any
        {' '}
        <InlineTextEditor text={step.variableName} callback={this.updateBoundVariable}/>
        .
      </ProofLine>
      <BoundVariableLists.Add variables={[step.variableName]}>
        <Steps.Children steps={step.substeps} path={path} />
      </BoundVariableLists.Add>
      {step.provenStatement &&
        <ProofLine.SingleStatementWithPrefix prefix="So"
                                             statement={step.provenStatement}
                                             path={path}
                                             additionalReferences={additionalReferences}
                                             premiseReferences={[new StepReference([...path, step.substeps.length - 1])]} />}
    </>
  }
});
