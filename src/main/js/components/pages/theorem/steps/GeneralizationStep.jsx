import React from "react";
import {FunctionParameter} from "../../../../models/Expression";

import {StepReference} from "../../../definitions/Reference";
import AddBoundVariableList from "../../../expressions/boundVariables/AddBoundVariableList";
import {HighlightableExpression} from "../../../expressions/ExpressionComponent";
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
    const referencesForVariableAndConclusion = showConclusion ? [] : [...(additionalReferences || []), reference];
    return <Step.WithSubsteps path={path}>
      <AddBoundVariableList variables={[step.variableName]}>
        <Step.Antecedent>
          <ProofLine path={path}>
            Take any <HighlightableExpression expression={new FunctionParameter(0, 0)}
                                              references={referencesForVariableAndConclusion}
                                              wrapBoundVariable={name => <InlineTextEditor text={name} callback={this.updateBoundVariable}/>}
            />.
          </ProofLine>
        </Step.Antecedent>
        <Steps.Children steps={step.substeps}
                        path={path}
                        propsForLastStep={{additionalReferences: referencesForVariableAndConclusion}} />
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
