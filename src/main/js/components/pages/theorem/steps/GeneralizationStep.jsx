import React from "react";

import {StepReference} from "../../../definitions/Reference";
import AddBoundVariableList from "../../../expressions/boundVariables/AddBoundVariableList";
import {InlineTextEditor} from "../../../helpers/InlineTextEditor";
import ProofContext from "../ProofContext";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

export class GeneralizationStep extends React.Component {
  static contextType = ProofContext;
  updateBoundVariable = (newName) => {
    return this.context.fetchJsonForStepAndReplace(this.props.path, "boundVariable", {method: "PUT", body: newName});
  };
  render() {
    let {step, path, additionalReferences, showConclusion} = this.props;
    const reference = new StepReference(path);
    const referencesForLastStep = showConclusion ? [] : [...(additionalReferences || []), reference];
    return <Step.WithSubsteps path={path}>
      <Step.Antecedent>
        <ProofLine path={path}>
          Take any <InlineTextEditor text={step.variableName} callback={this.updateBoundVariable}/>.
        </ProofLine>
      </Step.Antecedent>
      <AddBoundVariableList variables={[step.variableName]}>
        <Steps.Children steps={step.substeps}
                        path={path}
                        propsForLastStep={{additionalReferences: referencesForLastStep}} />
      </AddBoundVariableList>
      {step.provenStatement && showConclusion &&
        <ProofLine.SingleStatementWithPrefix prefix="So"
                                             statement={step.provenStatement}
                                             path={path}
                                             additionalReferences={additionalReferences}
                                             premiseReferences={[new StepReference([...path, step.substeps.length - 1])]} />}
    </Step.WithSubsteps>;
  }
};
