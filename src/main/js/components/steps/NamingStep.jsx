import React from "react";
import {connect} from "react-redux";
import ProofContext from "../theorem/ProofContext";
import {FetchJsonForStepAndUpdate} from "../theorem/TheoremStore";
import {InferenceLink} from "./InferenceLink";
import ProofLine from "./ProofLine";
import {Steps} from "./Steps";
import {InlineTextEditor} from "../helpers/InlineTextEditor";

export const NamingStep = connect()(class extends React.Component {
  static contextType = ProofContext;
  updateBoundVariable = (newName) => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "boundVariable", {method: "PUT", body: newName}));
  };
  render() {
    let {step, path, additionalReferences, boundVariableLists,} = this.props;
    let reference = {stepPath: path};
    let referenceForAssumption = {stepPath: path, suffix: "a"};
    let referencesForLastStep = [...additionalReferences, reference];
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    const prefix = <>
        Let
        {' '}
        <InlineTextEditor text={step.variableName} callback={this.updateBoundVariable}/>
        {' '}
        be such that
    </>;
    return <>
      <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                           prefix={prefix}
                                           statement={step.assumption}
                                           path={path}
                                           boundVariableLists={innerBoundVariableLists}
                                           additionalReferences={additionalReferences}
                                           premiseReferences={step.referencedLinesForExtraction}
                                           reference={referenceForAssumption}
                                           buttons={<InferenceLink inference={step.inference}/>} />
      <Steps steps={step.substeps}
             path={path}
             boundVariableLists={innerBoundVariableLists}
             referencesForLastStep={referencesForLastStep} />
    </>;
  }
});
