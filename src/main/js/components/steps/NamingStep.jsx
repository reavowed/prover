import React from "react";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";
import {InlineTextEditor} from "../helpers/InlineTextEditor";

export class NamingStep extends React.Component {
  updateBoundVariable = (newName) => {
    this.props.theoremContext
      .fetchJsonForStep(this.props.path, "boundVariable", {method: "PUT", body: newName})
      .then(this.props.theoremContext.updateTheorem);
  };
  render() {
    let {step, path, additionalReferences, theoremContext, boundVariableLists,} = this.props;
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
                                           buttons={<InferenceLink inference={step.inference}/>}
                                           theoremContext={theoremContext} />
      <Steps steps={step.substeps}
             path={path}
             boundVariableLists={innerBoundVariableLists}
             referencesForLastStep={referencesForLastStep}
             theoremContext={theoremContext} />
    </>;
  }
}
