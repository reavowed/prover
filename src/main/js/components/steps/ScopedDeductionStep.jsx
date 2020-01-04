import update from 'immutability-helper';
import React from "react";
import {connect} from "react-redux";
import {DefinedExpression} from "../../models/Expression";
import {StepReference} from "../../models/Step";
import {HighlightableExpression} from "../ExpressionComponent";
import {InlineTextEditor} from "../helpers/InlineTextEditor";
import ProofContext from "../theorem/ProofContext";
import {FetchJsonForStepAndUpdate} from "../theorem/TheoremStore";
import BoundVariableLists from "./BoundVariableLists";
import ProofLine from "./ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

export default connect()(class ScopedDeductionStep extends React.Component {
  static contextType = ProofContext;
  render() {
    let {step, path, additionalReferences, format, components, dispatch} = this.props;
    const substep = step.substeps[0];
    const substepPath = [...path, 0];

    const assumptionReference = new StepReference(substepPath, "a");

    const wrapBoundVariable = (name, index, boundVariablePath) => {
      const callback = (boundVariablePath.length === 0 && index === 0) ?
        (newName) => dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "boundVariable", {method: "PUT", body: newName})) :
        (newName) => dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, substepPath, `boundVariables/${update(boundVariablePath, {$splice: [[0, 1, boundVariablePath[0] + 1]]}).join(".")}/${index}/`, {method: "PUT", body: newName}));
      return <InlineTextEditor text={name} callback={callback} />;
    };

    const patchedExpression = new DefinedExpression({baseFormatString: format}, [step.variableName], components.slice(1));

    return <Step.WithSubsteps path={path}>
      <BoundVariableLists.Add variables={[step.variableName]}>
        <Step.Antecedent>
            <ProofLine path={path}>
              Take any <HighlightableExpression expression={patchedExpression} references={[assumptionReference]} wrapBoundVariable={wrapBoundVariable} expressionToCopy={substep.assumption}/>.
            </ProofLine>
        </Step.Antecedent>
        <Steps.Children steps={substep.substeps} path={substepPath} />
      </BoundVariableLists.Add>
      {step.provenStatement &&
      <ProofLine.SingleStatementWithPrefix prefix="So"
                                           statement={step.provenStatement}
                                           path={path}
                                           additionalReferences={additionalReferences}
                                           premiseReferences={[assumptionReference, new StepReference([...substepPath, substep.substeps.length - 1])]} />}
    </Step.WithSubsteps>;
  }
});
