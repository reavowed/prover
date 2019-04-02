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
                 path={path}
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
  constructor(...args) {
    super(...args)
    this.state = {
      boundVariableModalCallback: null,
      boundVariableName: ""
    }
  }
  showBoundVariableModal = (boundVariableName, boundVariableIndex, boundVariablePath) => {
    this.setState({
      boundVariableName,
      boundVariableModalCallback: () => this.updateBoundVariable(boundVariableIndex, boundVariablePath)
    })
  };
  hideBoundVariableModal = () => {
    this.setState({
      boundVariableModalCallback: null
    })
  };
  updateBoundVariable = (boundVariableIndex, boundVariablePath) => {
    this.props.apiService.fetchJsonForStep(this.props.path, `boundVariables/${boundVariablePath.join(".")}/${boundVariableIndex}/`, {
      method: "PUT",
      body: this.state.boundVariableName
    })
      .then(this.props.apiService.updateTheorem)
      .then(this.hideBoundVariableModal);
  };
  render() {
    let {step, boundVariableLists, additionalReferences, highlighting, path} = this.props;
    const wrapEditableBoundVariable = (boundVariableContent, boundVariableName, boundVariableIndex, boundVariablePath) =>
      <ClickableText
        onClick={() => this.showBoundVariableModal(boundVariableName, boundVariableIndex, boundVariablePath)}>
        {boundVariableContent}
      </ClickableText>;
    return <ElidedStepProofLine {...this.props}>
      Then <HighlightableExpression statement={step.statement}
                                    boundVariableLists={boundVariableLists}
                                    wrapBoundVariable={wrapEditableBoundVariable}
                                    references={[...additionalReferences, {stepPath: path}]}
                                    highlighting={highlighting}/>.
      <BoundVariableModal show={this.state.boundVariableModalCallback != null}
                          onHide={this.hideBoundVariableModal}
                          title="Rename bound variable"
                          value={this.state.boundVariableName}
                          onChange={e => this.setState({boundVariableName: e.target.value})}
                          onSave={this.state.boundVariableModalCallback}/>
    </ElidedStepProofLine>
  }
};
