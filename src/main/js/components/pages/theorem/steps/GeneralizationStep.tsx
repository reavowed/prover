import React, {useContext} from "react";
import {FunctionParameter} from "../../../../models/Expression";
import {StepReference} from "../../../definitions/Reference";
import AddBoundVariableList from "../../../expressions/boundVariables/AddBoundVariableList";
import {HighlightableExpression} from "../../../expressions/ExpressionComponent";
import {InlineTextEditor} from "../../../helpers/InlineTextEditor";
import ProofContext from "../ProofContext";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import Steps, {StepProps} from "./Steps";
import {GeneralizationStep as GeneralizationStepModel} from "../../../../models/Step";

export function GeneralizationStep({step, additionalReferences, showConclusion}: StepProps<GeneralizationStepModel>){
  const context = useContext(ProofContext)!;
  const updateBoundVariable = (newName: string) => {
    return context.fetchJsonForStepAndReplace(step.path, "boundVariable", {method: "PUT", body: newName});
  };
  const reference = new StepReference(step.path);
  const referencesForVariableAndConclusion = showConclusion ? [] : [...(additionalReferences || []), reference];
  return <Step.WithSubsteps path={step.path}>
    <AddBoundVariableList variables={[step.variableName]}>
      <Step.Antecedent>
        <ProofLine path={step.path}>
          Take any <HighlightableExpression expression={new FunctionParameter(0, 0)}
                                            references={referencesForVariableAndConclusion}
                                            wrapBoundVariable={name => <InlineTextEditor text={name} callback={updateBoundVariable}/>}
          />.
        </ProofLine>
      </Step.Antecedent>
      <Steps.Children steps={step.substeps}
                      path={step.path}
                      propsForLastStep={{additionalReferences: referencesForVariableAndConclusion}} />
    </AddBoundVariableList>
    {step.provenStatement && showConclusion &&
      <ProofLine.SingleStatementWithPrefix prefix="So"
                                           statement={step.provenStatement}
                                           path={step.path}
                                           additionalReferences={additionalReferences}
                                           premiseReferences={[new StepReference([...step.path, step.substeps.length - 1])]} />}
  </Step.WithSubsteps>;
};
