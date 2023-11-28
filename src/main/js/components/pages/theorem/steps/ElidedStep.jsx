import React from "react";
import {Button, InputGroup} from "react-bootstrap";
import Dropdown from "react-bootstrap/Dropdown";
import DropdownButton from "react-bootstrap/DropdownButton";
import Form from "react-bootstrap/Form";
import {ElidedStep as ElidedStepModel} from "../../../../models/Step";
import ProofContext from "../ProofContext";
import TheoremContext from "../TheoremContext";
import {InferenceLink} from "./components/InferenceLink";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

class ElidedStepProofLineWithContexts extends React.Component {
  constructor(props) {
    super(props);
    const containsHighlightedInference = _.intersection(_.map(props.step.inferencesUsed, "id"), props.theoremContext.inferencesToHighlight).length;
    const isHighlightedInference = props.step.highlightedInference && _.isEqual(props.theoremContext.inferencesToHighlight, [props.step.highlightedInference.id]);
    this.proofLineRef = React.createRef();
    this.state = {
      showProofCard: !props.step.isComplete || (containsHighlightedInference && !isHighlightedInference),
      draftDescription: "",
      savingDescription: false
    }
  }

  toggleProofCard = () => {
    this.setStatePromise({showProofCard: !this.state.showProofCard})
      .then(() => this.proofLineRef.current && this.proofLineRef.current.focus());
  };

  setDescription = (description) => {
    this.setStatePromise({savingDescription: true})
      .then(() => this.props.proofContext.fetchJsonForStepAndReplace(this.props.step.path, "description", {
        method: "POST",
        body: description
      }))
      .catch(() => {})
      .then(() => this.setStatePromise({savingDescription: false}));
  };

  highlightInference = (inferenceId) => {
    this.props.proofContext.fetchJsonForStepAndReplace(this.props.step.path, "highlightedInference", {
      method: "POST",
      body: inferenceId
    });
  };

  unpackStep = () => {
    this.props.proofContext.fetchJsonForStepAndReplace(this.props.step.path, "unpack", {method: "POST"});
  };

  onProofLineKeyDown = (event) => {
    if (event.key === "x") {
      this.toggleProofCard();
    } else if (event.key === "u") {
      this.unpackStep();
    }
  };

  render() {
    let {step, children} = this.props;
    let {draftDescription, savingDescription} = this.state;
    let buttons = <>
      {step.highlightedInference && <span className="mr-2"><InferenceLink inference={step.highlightedInference}/></span>}
      {step.description && <span className="text-muted text-uppercase mr-2" style={{"fontFamily": "monospace"}}>{step.description}</span>}
      {!step.highlightedInference && !step.description && step.inferencesUsed.length > 0 && <DropdownButton title="Highlighted Inference" size="sm" className="mr-2" as="span">
        {_.chain(step.getAllSubsteps()).filter(s => (s instanceof ElidedStepModel)).map(s => s.description).filter().uniq().value().map(d => <Dropdown.Item key={d} onClick={() => this.setDescription(d)}>{d}</Dropdown.Item>)  }
        {_.uniqBy(step.inferencesUsed, "id").map(i => <Dropdown.Item key={i.id} onClick={() => this.highlightInference(i.id)}>{i.name}</Dropdown.Item>)}
      </DropdownButton>}
      {!step.highlightedInference && !step.description && <InputGroup className="mr-2 d-inline-flex w-auto">
        <Form.Control type="text" readOnly={savingDescription} value={draftDescription} onChange={e => this.setState({draftDescription: e.target.value})}/>
        <InputGroup.Append><Button variant="success" disabled={savingDescription} onClick={() => this.setDescription(draftDescription)}><span className={savingDescription ? "fas fa-spin fa-spinner" : "fas fa-check"}/></Button></InputGroup.Append>
      </InputGroup>}
      <span className="fas fa-ellipsis-v text-muted mr-2" onClick={this.toggleProofCard} style={{cursor: "pointer"}}/>
    </>;
    const proofLine = <ProofLine premiseReferences={step.filterReferences(step.path)}
                                 path={step.path}
                                 buttons={buttons}
                                 incomplete={!step.isComplete}
                                 onKeyDown={this.onProofLineKeyDown}
                                 ref={this.proofLineRef}
    >
      {children}
    </ProofLine>;
    return <>
      {this.state.showProofCard ?
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
}

export function ElidedStepProofLine(props) {
  return <ProofContext.Consumer>{proofContext =>
    <TheoremContext.Consumer>{theoremContext =>
      <ElidedStepProofLineWithContexts {...props} proofContext={proofContext} theoremContext={theoremContext}/>
    }</TheoremContext.Consumer>
  }</ProofContext.Consumer>
}

export function ElidedStep(props) {
  const {step, additionalReferences} = props;
  return <ElidedStepProofLine {...props} prefix="Then">
    <ProofLine.SingleStatementWithPrefixContent prefix="Then"
                                                statement={step.provenStatement}
                                                path={step.path}
                                                additionalReferences={additionalReferences}  />
  </ElidedStepProofLine>;
}
