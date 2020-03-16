import React, {useContext} from "react";
import {StepReference} from "../../../../models/Step";
import ProofContext from "../ProofContext";
import {InferenceLink} from "./components/InferenceLink";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";
import {InlineTextEditor} from "../../../helpers/InlineTextEditor";
import BoundVariableLists from "./BoundVariableLists";

export function NamingStep({step, path, additionalReferences}) {
  const context = useContext(ProofContext);
  const updateBoundVariable = (newName) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "boundVariable", {method: "PUT", body: newName});
  };
  const reference = new StepReference(path);
  const prefix = <>
      Let
      {' '}
      <InlineTextEditor text={step.variableName} callback={updateBoundVariable}/>
      {' '}
      be such that
  </>;
  return <Step.WithSubsteps path={path}>
    <BoundVariableLists.Add variables={[step.variableName]}>
      <Step.Antecedent>
        <ProofLine.SingleStatementWithPrefix editableBoundVariable
                                             prefix={prefix}
                                             statement={step.assumption}
                                             path={path}
                                             suffix="a"
                                             additionalReferences={additionalReferences}
                                             premiseReferences={step.referencedLinesForExtraction}
                                             buttons={<InferenceLink inference={step.inference}/>} />
      </Step.Antecedent>
      <Steps steps={step.substeps}
             path={path}
             propsForLastStep={{additionalReferences: [...additionalReferences, reference]}} />
    </BoundVariableLists.Add>
  </Step.WithSubsteps>;
}
