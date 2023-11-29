import React, {useContext, useEffect, useRef, useState} from "react";
import {Button, InputGroup} from "react-bootstrap";
import Dropdown from "react-bootstrap/Dropdown";
import DropdownButton from "react-bootstrap/DropdownButton";
import Form from "react-bootstrap/Form";
import {ElidedStep as ElidedStepModel, Step as StepModel} from "../../../../models/Step";
import ProofContext from "../ProofContext";
import TheoremContext from "../TheoremContext";
import {InferenceLink} from "./components/InferenceLink";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import Steps, {StepProps} from "./Steps";
import _ from "lodash";
import {isDefined} from "../../../../utils";

export function ElidedStepProofLine({step, children}: React.PropsWithChildren<StepProps<ElidedStepModel>>) {
  const proofContext = useContext(ProofContext)!;
  const theoremContext = useContext(TheoremContext)!;
    const [showProofCard, setShowProofCard] = useState(() => {
    const containsHighlightedInference = !!theoremContext.inferencesToHighlight && _.intersection(_.map(step.inferencesUsed, "id"), theoremContext.inferencesToHighlight).length > 0;
    const isHighlightedInference = !!step.highlightedInference && _.isEqual(theoremContext.inferencesToHighlight, [step.highlightedInference.id]);
    return !step.isComplete || (containsHighlightedInference && !isHighlightedInference);
  });
  const [draftDescription, setDraftDescription] = useState('');
  const [savingDescription, setSavingDescription] = useState(false);
  const proofLineRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (showProofCard && proofLineRef.current) {
      proofLineRef.current.focus();
    }
  }, [showProofCard, proofLineRef.current]);

  const setDescription = (description: string) => {
    setSavingDescription(true);
    proofContext.fetchJsonForStepAndReplace(step.path, "description", {
      method: "POST",
      body: description
    })
      .catch(() => {})
      .then(() => setSavingDescription(false));
  }

  const highlightInference = (inferenceId: string) => {
    proofContext.fetchJsonForStepAndReplace(step.path, "highlightedInference", {
      method: "POST",
      body: inferenceId
    });
  }

  const unpackStep = () => {
    proofContext.fetchJsonForStepAndReplace(step.path, "unpack", {
      method: "POST"
    });
  }

  const toggleProofCard = () => setShowProofCard(v => !v);

  const onProofLineKeyDown = (event: React.KeyboardEvent<HTMLElement>) => {
    if (event.key === "x") {
      toggleProofCard();
    } else if (event.key === "u") {
      unpackStep();
    }
  }

  function isElidedStep(step: StepModel): step is ElidedStepModel {
    return step instanceof ElidedStepModel;
  }

  const buttons = <>
    {step.highlightedInference && <span className="mr-2"><InferenceLink inference={step.highlightedInference}/></span>}
    {step.description && <span className="text-muted text-uppercase mr-2" style={{"fontFamily": "monospace"}}>{step.description}</span>}
    {!step.highlightedInference && !step.description && step.inferencesUsed.length > 0 && <DropdownButton title="Highlighted Inference" size="sm" className="mr-2" as="span">
      {_.chain(step.getAllSubsteps()).filter(isElidedStep).map(s => s.description).filter(isDefined).uniq().value().map(d => <Dropdown.Item key={d} onClick={() => setDescription(d)}>{d}</Dropdown.Item>)  }
      {_.uniqBy(step.inferencesUsed, "id").map(i => <Dropdown.Item key={i.id} onClick={() => highlightInference(i.id)}>{i.name}</Dropdown.Item>)}
    </DropdownButton>}
    {!step.highlightedInference && !step.description && <InputGroup className="mr-2 d-inline-flex w-auto">
      <Form.Control type="text" readOnly={savingDescription} value={draftDescription} onChange={e => setDraftDescription(e.target.value)}/>
      <InputGroup.Append><Button variant="success" disabled={savingDescription} onClick={() => setDescription(draftDescription)}><span className={savingDescription ? "fas fa-spin fa-spinner" : "fas fa-check"}/></Button></InputGroup.Append>
    </InputGroup>}
    <span className="fas fa-ellipsis-v text-muted mr-2" onClick={toggleProofCard} style={{cursor: "pointer"}}/>
  </>;
  const proofLine = <ProofLine premiseReferences={step.referencedLines}
                               path={step.path}
                               buttons={buttons}
                               incomplete={!step.isComplete}
                               onKeyDown={onProofLineKeyDown}
                               ref={proofLineRef}
  >
    {children}
  </ProofLine>;
  return <>
    {showProofCard ?
      <Step.WithSubsteps path={step.path}>
        <Step.Antecedent>{proofLine}</Step.Antecedent>
        <div className="card" style={{margin: ".5rem -0.75rem .5rem 2rem", padding: ".5rem .75rem"}}>
          <Steps.Children steps={step.substeps}
                          path={step.path}
                          propsForLastStep={{showConclusion: true}}/>
        </div>
      </Step.WithSubsteps> :
      <Step.WithoutSubsteps>{proofLine}</Step.WithoutSubsteps>}
  </>;
}

export function ElidedStep(props: StepProps<ElidedStepModel>) {
  const {step, additionalReferences} = props;
  return <ElidedStepProofLine {...props}>
    <ProofLine.SingleStatementWithPrefixContent prefix="Then"
                                                statement={step.provenStatement}
                                                path={step.path}
                                                additionalReferences={additionalReferences}  />
  </ElidedStepProofLine>;
}
