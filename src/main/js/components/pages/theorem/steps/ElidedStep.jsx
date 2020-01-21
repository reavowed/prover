import React from "react";
import Dropdown from "react-bootstrap/Dropdown";
import DropdownButton from "react-bootstrap/DropdownButton";
import {ElidedStep as ElidedStepModel} from "../../../../models/Step";
import ProofContext from "../ProofContext";
import {InferenceLink} from "./components/InferenceLink";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

export class ElidedStepProofLine extends React.Component {
  static contextType = ProofContext;
  constructor(...args) {
    super(...args);
    this.state = {
      showProofCard: !this.props.step.isComplete
    }
  }

  componentDidUpdate(prevProps, prevState, snapshot) {
    if (this.props.step.isComplete && !prevProps.step.isComplete) {
      this.setState({showProofCard: false});
    }
  }

  toggleProofCard = () => {
    this.setState({showProofCard: !this.state.showProofCard})
  };

  setDescription = (description) => {
    this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "description", {
      method: "POST",
      body: description
    });
  };

  highlightInference = (inferenceId) => {
    this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "highlightedInference", {
      method: "POST",
      body: inferenceId
    });
  };

  render() {
    let {step, path, children, standalone} = this.props;
    let buttons = <>
      {step.highlightedInference && <InferenceLink inference={step.highlightedInference} suffix={<span className="fas fa-ellipsis-v"/>}/>}
      {step.description && <span className="text-muted text-uppercase ml-1" style={{"fontFamily": "monospace"}}>{step.description} <span className="fas fa-ellipsis-v"/></span>}
      {!step.highlightedInference && !step.description && <DropdownButton title="Highlighted Inference" size="sm" className="ml-1">
        {_.chain(step.allSubsteps).filter(s => (s instanceof ElidedStepModel)).map(s => s.description).filter().uniq().value().map(d => <Dropdown.Item key={d} onClick={() => this.setDescription(d)}>{d}</Dropdown.Item>)  }
        {_.uniqBy(step.inferencesUsed, "id").map(i => <Dropdown.Item key={i.id} onClick={() => this.highlightInference(i.id)}>{i.name}</Dropdown.Item>)}
      </DropdownButton>}
    </>;
    const proofLine = <ProofLine premiseReferences={_.filter(step.referencedLines, ({stepPath}) => !stepPath || !_.startsWith(stepPath, path))}
                                 path={path}
                                 statement={step.statement}
                                 buttons={buttons}
                                 onClick={this.toggleProofCard}
                                 incomplete={!step.isComplete}
    >
      {children}
    </ProofLine>;
    return <>
      {this.state.showProofCard ?
        <Step.WithSubsteps path={path}>
          <Step.Antecedent>{proofLine}</Step.Antecedent>
          <div className="card" style={{margin: ".5rem -0.75rem .5rem 2rem", padding: ".5rem .75rem"}}>
            <Steps.Children steps={step.substeps}
                            path={path}
                            referencesForLastStep={[]}/>
          </div>
        </Step.WithSubsteps> :
        standalone ? proofLine :
        <Step.WithoutSubsteps>{proofLine}</Step.WithoutSubsteps>}
    </>;
  }
}

export class ElidedStep extends React.Component {
  render() {
    const {step, path} = this.props;
    return <ElidedStepProofLine standalone {...this.props} prefix="Then">
      <ProofLine.SingleStatementWithPrefixContent prefix="Then"
                                                  statement={step.statement}
                                                  path={path} />
    </ElidedStepProofLine>;
  }
}
