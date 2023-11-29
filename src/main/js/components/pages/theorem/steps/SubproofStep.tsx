import React, {useContext, useState} from "react";
import styled from "styled-components";

import {StepReference} from "../../../definitions/Reference";
import {formatHtml} from "../../../helpers/Formatter";
import ProofContext from "../ProofContext";
import TheoremContext from "../TheoremContext";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import Steps, {StepProps} from "./Steps";
import {SubproofStep as SubproofStepModel} from "../../../../models/Step";
import _ from "lodash"

const SubproofOutline = styled.div`
  border: 1px solid black;
  border-radius: .25rem;
  padding: 0.25rem 0.5rem 0.25rem 0.5rem;
  margin-bottom: 0.25rem;
`;

export function SubproofStep({step, additionalReferences = []}: StepProps<SubproofStepModel>) {
  const proofContext = useContext(ProofContext)!;
  const theoremContext = useContext(TheoremContext)!;

  const [showingSubproof, setShowingSubproof] = useState(() =>
    !step.isComplete && (step.substeps.length > 1 || step.substeps[0].type !== "target") || _.intersection(_.map(step.inferencesUsed, "id"), theoremContext.inferencesToHighlight || []).length > 0
  );
  const toggleSubproof = () => setShowingSubproof(v => !v);
  const unpackStep = () => proofContext.fetchJsonForStepAndReplace(step.path, "unpack", {method: "POST"});
  const onProofLineKeyDown = (event: React.KeyboardEvent<HTMLElement>) => {
    if (event.key === "u") {
      unpackStep();
    }
  };
  let reference = new StepReference(step.path);
  const titleElement = <div onClick={toggleSubproof} className={"font-weight-bold mt-1 mb-1"} style={{cursor: "pointer"}}>{formatHtml(step.name)}</div>;
  return showingSubproof ?
    <SubproofOutline>
      <Step.WithSubsteps path={step.path}>
        <Step.Antecedent>{titleElement}</Step.Antecedent>
        <Steps steps={step.substeps}
               path={step.path}
               propsForLastStep={{additionalReferences: [...additionalReferences, reference], showConclusion: true}} />
      </Step.WithSubsteps>
    </SubproofOutline> :
    <SubproofOutline>
      <Step.WithoutSubsteps>
        {titleElement}
        <ProofLine path={step.path}
                   premiseReferences={step.referencedLines}
                   incomplete={!step.isComplete}
                   onKeyDown={onProofLineKeyDown}>
          <ProofLine.SingleStatementWithPrefixContent prefix="Then"
                                                      statement={step.provenStatement}
                                                      path={step.path} />
        </ProofLine>
      </Step.WithoutSubsteps>
    </SubproofOutline>;
}
