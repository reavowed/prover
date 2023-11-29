import _ from "lodash";
import React, {useContext, useEffect, useState} from "react";
import Button from "react-bootstrap/Button";
import AvailableEntriesContext from "../../../AvailableEntriesContext";
import ProofContext from "../ProofContext";
import ProofLine from "./components/ProofLine";
import ProvingCard from "./proving/ProvingCard";
import Step from "./Step";
import {StepProps} from "./Steps";
import {TargetStep as TargetStepModel} from "../../../../models/Step";
import {Premise} from "../../../definitions/Premise";

export function TargetStepProofLine({step, children}: React.PropsWithChildren<StepProps<TargetStepModel>>) {
  const [proving, setProving] = useState(false);
  const [availablePremises, setAvailablePremises] = useState<Premise[]>([]);
  const proofContext = useContext(ProofContext)!;
  const availableEntries = useContext(AvailableEntriesContext);

  const startProving = () => setProving(true);
  const stopProving = () => setProving(false);
  useEffect(() => {
    const actions = {
      onUpdate() {
        proofContext.fetchJsonForStep(step.path, "premises")
          .then(premiseJson => setAvailablePremises(_.map(premiseJson, availableEntries.parser.parsePremise)))
          .catch(console.log);
      },
      startProving,
      stopProving
    }
    proofContext.registerStep(actions, step.path);
    return () => proofContext.unregisterStep(actions, step.path);
  }, [step.path, availableEntries])

  const onProofLineKeyDown = (event: React.KeyboardEvent<HTMLElement>) => {
    if (event.key === "p") {
      startProving();
    }
  };

  return proving ?
    <ProvingCard step={step} path={step.path} availablePremises={availablePremises} stopProving={stopProving} /> :
    <ProofLine incomplete
               path={step.path}
               onKeyDown={onProofLineKeyDown}
               buttons={<Button variant="danger" size="sm" className="pt-0 pb-0" onClick={startProving}>Prove</Button>}>
      {children}
    </ProofLine>;
}

export function TargetStep(props: StepProps<TargetStepModel>) {
  const {step, additionalReferences} = props;
  return <Step.WithoutSubsteps>
    <TargetStepProofLine {...props}>
      <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                  prefix="Then"
                                                  statement={step.statement}
                                                  path={step.path}
                                                  additionalReferences={additionalReferences} />
    </TargetStepProofLine>
  </Step.WithoutSubsteps>;
}
