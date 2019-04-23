import React from "react";
import Dropdown from "react-bootstrap/Dropdown";
import DropdownButton from "react-bootstrap/DropdownButton";
import {HighlightableExpression} from "../ExpressionComponent";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";
import {ClickableText} from "./ClickableText";
import {BoundVariableModal} from "../Modals";
import {AssertionStepProofLine} from "./AssertionStep";

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
    let buttons = <>
      {step.highlightedInference && <InferenceLink inference={step.highlightedInference} suffix="[elided]"/>}
      {!step.highlightedInference && <DropdownButton title="Highlighted Inference" size="sm" className="ml-1">
        {step.inferencesUsed.map(i => <Dropdown.Item key={i.id} onClick={() => this.highlightInference(i.id)}>{i.name}</Dropdown.Item>)}
      </DropdownButton>}
    </>;
    return <>
      <ProofLine premiseReferences={_.filter(step.referencedLines, ({stepPath}) => !stepPath || !_.startsWith(stepPath, path))}
                 boundVariableLists={boundVariableLists}
                 path={path}
                 statement={step.statement}
                 buttons={buttons}
                 onClick={this.toggleProofCard}
                 apiService={apiService}
                 highlighting={highlighting}
                 incomplete={step.isIncomplete}
      >
        {children}
      </ProofLine>
      {this.state.showProofCard && <div className="card" style={{margin: ".5rem -0.75rem .5rem 2rem", padding: ".5rem .75rem"}}>
        <Steps steps={step.substeps}
               path={path}
               boundVariableLists={boundVariableLists}
               referencesForLastStep={[]}
               apiService={apiService}
               highlighting={highlighting}/>
      </div>}
    </>;
  }
}

export class ElidedStep extends React.Component {
  render() {
    const {step, path, boundVariableLists, highlighting, apiService} = this.props;
    return <ElidedStepProofLine {...this.props} prefix="Then">
      <ProofLine.SingleStatementWithPrefixContent prefix="Then"
                                                  statement={step.statement}
                                                  path={path}
                                                  boundVariableLists={boundVariableLists}
                                                  highlighting={highlighting}
                                                  apiService={apiService} />
    </ElidedStepProofLine>;
  }
}
