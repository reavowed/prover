import React from "react";
import {connect} from "react-redux";
import {StepReference} from "../../models/Step";
import ProofContext from "../theorem/ProofContext";
import {FetchJsonForStepAndUpdate} from "../theorem/TheoremStore";
import {InferenceLink} from "./InferenceLink";
import ProofLine from "./ProofLine";
import {Steps} from "./Steps";
import {InlineTextEditor} from "../helpers/InlineTextEditor";
import BoundVariableLists from "./BoundVariableLists";

export const NamingStep = connect()(class NamingStep extends React.Component {
  static contextType = ProofContext;
  updateBoundVariable = (newName) => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "boundVariable", {method: "PUT", body: newName}));
  };
  render() {
    let {step, path, additionalReferences} = this.props;
    let reference = new StepReference(path);
    let referencesForLastStep = [...additionalReferences, reference];
    const prefix = <>
        Let
        {' '}
        <InlineTextEditor text={step.variableName} callback={this.updateBoundVariable}/>
        {' '}
        be such that
    </>;
    return <BoundVariableLists.Add variables={[step.variableName]}>
      <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                           prefix={prefix}
                                           statement={step.assumption}
                                           path={path}
                                           suffix="a"
                                           additionalReferences={additionalReferences}
                                           premiseReferences={step.referencedLinesForExtraction}
                                           buttons={<InferenceLink inference={step.inference}/>} />
      <Steps steps={step.substeps}
             path={path}
             referencesForLastStep={referencesForLastStep} />
    </BoundVariableLists.Add>;
  }
});
