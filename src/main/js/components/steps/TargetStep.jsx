import React from "react";
import Button from "react-bootstrap/Button";
import styled from "styled-components";
import {HighlightableExpression} from "../ExpressionComponent";
import {BoundVariableModal, FindInferenceModal} from "../Modals";
import {ProofLine} from "./ProofLine";

const EditableBoundVariable = styled.span`
  &:hover {
    color: red;
    cursor: pointer;
  }
`;

export class TargetStep extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      boundVariableModalCallback: null,
      showFindInferenceModal: false,
      boundVariableName: ""
    };
  }

  isShowingBoundVariableModal = () => this.state.boundVariableModalCallback != null;
  hideBoundVariableModal = () => {
    this.setState({boundVariableModalCallback: null})
  };
  updateBoundVariableName = (event) => {
    this.setState({boundVariableName: event.target.value})
  };

  startIntroducingBoundVariable= () => {
    this.setState({
      boundVariableModalCallback: this.introduceBoundVariable,
      boundVariableName: props.step.statement.boundVariableNames && props.step.statement.boundVariableNames[0] || ""
    });
  };
  introduceBoundVariable = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "introduceBoundVariable", {
      method: "POST",
      body: this.state.boundVariableName
    }).then(this.props.apiService.updateTheorem);
  };

  startUpdatingBoundVariable = (boundVariableName, boundVariableIndex, boundVariablePath) => {
    this.setState({
      boundVariableModalCallback: () => this.updateBoundVariable(boundVariableIndex, boundVariablePath),
      boundVariableName
    });
  };
  updateBoundVariable = (boundVariableIndex, boundVariablePath) => {
    this.props.apiService.fetchJsonForStep(this.props.path, `boundVariables/${boundVariablePath.join(".")}/${boundVariableIndex}/`, {
      method: "PUT",
      body: this.state.boundVariableName
    })
      .then(this.props.apiService.updateTheorem)
      .then(this.hideBoundVariableModal);
  };

  showFindInferenceModal = () => {
    this.setState({showFindInferenceModal: true})
  };

  hideFindInferenceModal = () => {
    this.setState({showFindInferenceModal: false})
  };

  getInferenceSuggestions = (searchText) => {
    return this.props.apiService.fetchJsonForStep(this.props.path, `suggestInferences?searchText=${searchText}`)
  };
  getPremiseSuggestions = (inferenceId) => {
    return this.props.apiService.fetchJsonForStep(this.props.path, `suggestPremises?inferenceId=${inferenceId}`)
  };

  introduceDeduction = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "introduceDeduction", {
      method: "POST"
    }).then(this.props.apiService.updateTheorem);
  };

  elide = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "elide", {
      method: "POST"
    }).then(this.props.apiService.updateTheorem);
  };

  proveWithInference = (inferenceId, substitutions) => {
    this.props.apiService.fetchJsonForStep(this.props.path, "", {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions})
    }).then(this.props.apiService.updateTheorem);
  };

  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists} = this.props;
    let reference = path.join(".");
    let scopingStatement = _.find(window.definitions, d => d.structureType === "scoping");
    let deductionStatement = _.find(window.definitions, d => d.structureType === "deduction");

    const boundVariableModal = <BoundVariableModal show={this.isShowingBoundVariableModal()}
                                                   onHide={this.hideBoundVariableModal}
                                                   title="Introduce bound variable"
                                                   value={this.state.boundVariableName}
                                                   onChange={this.updateBoundVariableName}
                                                   onSave={this.state.boundVariableModalCallback}/>;
    const wrapEditableBoundVariable = (boundVariableContent, boundVariableName, boundVariableIndex, boundVariablePath) =>
      <EditableBoundVariable
        onClick={() => this.startUpdatingBoundVariable(boundVariableName, boundVariableIndex, boundVariablePath)}>
        {boundVariableContent}
      </EditableBoundVariable>;

    const buttons = (
      <>
        <Button variant="success" size="sm" onClick={this.showFindInferenceModal}>Find inference</Button>
        <Button variant="success" size="sm" className="ml-1" onClick={this.elide}>Elide</Button>
        {scopingStatement && step.statement.definition === scopingStatement &&
        <Button variant="success" size="sm" className="ml-1" onClick={this.startIntroducingBoundVariable}>Introduce bound variable</Button>}
        {deductionStatement && step.statement.definition === deductionStatement &&
        <Button variant="success" size="sm" className="ml-1" onClick={this.introduceDeduction}>Introduce deduction</Button>}
      </>
    );
    return <>
      <ProofLine incomplete
                 path={path}
                 buttons={buttons}
                 blockHide={this.state.showFindInferenceModal}
                 apiService={apiService}
                 highlighting={highlighting}>
        Then <HighlightableExpression statement={step.statement}
                                      boundVariableLists={boundVariableLists}
                                      references={[...additionalReferences, reference]}
                                      wrapBoundVariable={wrapEditableBoundVariable}
                                      highlighting={highlighting}
        />.
      </ProofLine>
      {boundVariableModal}
      {<FindInferenceModal show={this.state.showFindInferenceModal}
                           onHide={this.hideFindInferenceModal}
                           onSubmit={this.proveWithInference}
                           getInferenceSuggestions={this.getInferenceSuggestions}
                           getPremiseSuggestions={this.getPremiseSuggestions}
                           boundVariableLists={boundVariableLists} />}
    </>
  }
}
