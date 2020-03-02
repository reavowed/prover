import React from "react";
import {StepReference} from "../../../../models/Step";
import {InlineTextEditor} from "../../../helpers/InlineTextEditor";
import ProofContext from "../ProofContext";
import BoundVariableLists from "./BoundVariableLists";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

export class GeneralizationStep extends React.Component {
  static contextType = ProofContext;
  updateBoundVariable = (newName) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "boundVariable", {method: "PUT", body: newName});
  };
  render() {
    let {step, path, additionalReferences} = this.props;
    return <Step.WithSubsteps path={path}>
      <Step.Antecedent>
        <ProofLine path={path}>
          Take any <InlineTextEditor text={step.variableName} callback={this.updateBoundVariable}/>.
        </ProofLine>
      </Step.Antecedent>
      <BoundVariableLists.Add variables={[step.variableName]}>
        <Steps.Children steps={step.substeps} path={path} />
      </BoundVariableLists.Add>
      {step.provenStatement &&
        <ProofLine.SingleStatementWithPrefix prefix="So"
                                             statement={step.provenStatement}
                                             path={path}
                                             additionalReferences={additionalReferences}
                                             premiseReferences={[new StepReference([...path, step.substeps.length - 1])]} />}
    </Step.WithSubsteps>;
  }
};
