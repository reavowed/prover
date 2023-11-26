import update from 'immutability-helper';
import React from "react";
import {DefinedExpression} from "../../../../models/Expression";
import {StepReference} from "../../../../models/Step";
import {HighlightableExpression} from "../../../expressions/ExpressionComponent";
import AddBoundVariableList from "../../../expressions/boundVariables/AddBoundVariableList";
import {InlineTextEditor} from "../../../helpers/InlineTextEditor";
import ProofContext from "../ProofContext";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

export default class GeneralizedDeductionStep extends React.Component {
  static contextType = ProofContext;
  shouldComponentUpdate() {
    return true;
  }
  render() {
    let {step, path, additionalReferences, variableDescription, showConclusion} = this.props;
    additionalReferences = additionalReferences || [];
    const substep = step.substeps[0];
    const substepPath = [...path, 0];

    const reference = new StepReference(path);
    const assumptionReference = new StepReference(substepPath, "a");

    const wrapBoundVariable = (name, index, boundVariablePath) => {
      const callback = (boundVariablePath.length === 0 && index === 0) ?
        (newName) => this.context.fetchJsonForStepAndReplace(this.props.path, "boundVariable", {method: "PUT", body: newName}) :
        (newName) => this.context.fetchJsonForStepAndReplace(substepPath, `boundVariables/${update(boundVariablePath, {$splice: [[0, 1, boundVariablePath[0] + 1]]}).join(".")}/${index}/`, {method: "PUT", body: newName});
      return <InlineTextEditor text={name} callback={callback} />;
    };

    const patchedExpression = new DefinedExpression(variableDescription.definition, [step.variableName], variableDescription.otherComponents);
    const referencesForLastStep = showConclusion ? [] : [...additionalReferences, reference];

    return <Step.WithSubsteps path={path}>
      <Step.Antecedent>
          <ProofLine path={path}>
            Take any <HighlightableExpression expression={patchedExpression} references={[assumptionReference]} additionalPremiseReferences={referencesForLastStep} wrapBoundVariable={wrapBoundVariable} expressionToCopy={substep.assumption}/>.
          </ProofLine>
      </Step.Antecedent>
      <AddBoundVariableList variables={[step.variableName]}>
        <Steps.Children steps={substep.substeps} path={substepPath} propsForLastStep={{additionalReferences: referencesForLastStep}} />
      </AddBoundVariableList>
      {step.provenStatement && showConclusion &&
        <ProofLine.SingleStatementWithPrefix prefix="So"
                                             statement={step.provenStatement}
                                             path={path}
                                             additionalReferences={additionalReferences}
                                             premiseReferences={[assumptionReference, new StepReference([...substepPath, substep.substeps.length - 1])]} />}
    </Step.WithSubsteps>;
  }
};
