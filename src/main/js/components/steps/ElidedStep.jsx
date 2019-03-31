import React from "react";
import Dropdown from "react-bootstrap/Dropdown";
import DropdownButton from "react-bootstrap/DropdownButton";
import {HighlightableExpression} from "../ExpressionComponent";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class ElidedStepProofLine extends React.Component {
  constructor(...args) {
    super(...args);
    this.state = {
      showProofCard: this.props.step.isIncomplete
    }
  }

  componentDidUpdate(prevProps, prevState, snapshot) {
    if (!this.props.step.isIncomplete && prevProps.step.isIncomplete) {
      this.setState({showProofCard: false});
    }
  }

  toggleProofCard = () => {
    this.setState({showProofCard: !this.state.showProofCard})
  };

  highlightInference = (inferenceId) => {
    this.props.apiService.fetchJsonForStep(this.props.path, "highlightedInference", {
      method: "POST",
      body: inferenceId
    }).then(this.props.apiService.updateTheorem);
  };

  render() {
    let {step, path, boundVariableLists, apiService, highlighting, children} = this.props;
    let reference = path.join(".");
    let buttons = <>
      {step.highlightedInference && <InferenceLink inference={step.highlightedInference} suffix="[elided]"/>}
      {!step.highlightedInference && <DropdownButton title="Highlighted Inference" size="sm" className="ml-1">
        {step.inferencesUsed.map(i => <Dropdown.Item key={i.id} onClick={() => this.highlightInference(i.id)}>{i.name}</Dropdown.Item>)}
      </DropdownButton>}
    </>;
    return <>
      <ProofLine premiseReferences={_.filter(step.referencedLines, r => !r.lineReference.startsWith(reference))}
                 path={path}
                 buttons={buttons}
                 onClick={this.toggleProofCard}
                 apiService={apiService}
                 highlighting={highlighting}
                 incomplete={step.isIncomplete}
      >
        {children}
      </ProofLine>
      {this.state.showProofCard && <div className="card" style={{margin: ".5rem 2rem", padding: ".5rem .75rem", display: "inline-block"}}>
        <Steps steps={step.substeps}
               elided
               path={path}
               boundVariableLists={boundVariableLists}
               referencesForLastStep={[]}
               apiService={apiService}
               highlighting={highlighting}/>
      </div>}
    </>;
  }
}

export const ElidedStep = (props) => {
  let {step, boundVariableLists, additionalReferences, highlighting, path} = props;
  return <ElidedStepProofLine {...props}>
    Then <HighlightableExpression statement={step.statement}
                                  boundVariableLists={boundVariableLists}
                                  references={[...additionalReferences, path.join(".")]}
                                  highlighting={highlighting}/>.
  </ElidedStepProofLine>
};
