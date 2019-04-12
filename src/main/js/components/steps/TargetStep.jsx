import React from "react";
import Button from "react-bootstrap/Button";
import _ from "lodash";
import {HighlightableExpression} from "../ExpressionComponent";
import {BoundVariableModal, FindInferenceModal} from "../Modals";
import {ClickableText} from "./ClickableText";
import {ProofLine} from "./ProofLine";

export class TargetStep extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      boundVariableModalCallback: null,
      findInferenceModalCallbacks: null,
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
      boundVariableModalTitle: "Introduce bound variable",
      boundVariableModaLabel: "Bound variable name",
      boundVariableModalCallback: this.introduceBoundVariable,
      boundVariableName: this.props.step.statement.boundVariableNames && this.props.step.statement.boundVariableNames[0] || ""
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
      boundVariableModalTitle: "Rename bound variable",
      boundVariableModalLabel: "Bound variable name",
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

  shouldShowFindInferenceModal = () => this.state.findInferenceModalCallbacks != null;

  findInferenceForAssertion = () => {
    this.setState({findInferenceModalCallbacks: {
        getInferenceSuggestions: this.getStepInferenceSuggestions,
        getPremiseSuggestions: this.getPremiseSuggestions,
        submit: this.proveWithInference
      }})
  };

  chooseBoundVariableForNaming = () => {
    this.setState({
      boundVariableModalTitle: "Choose variable name",
      boundVariableModalLabel: "Variable name",
      boundVariableModalCallback: () => this.findInferenceForNaming(),
      boundVariableName: ''
    });
  };
  findInferenceForNaming = () => {
    this.setState({
      boundVariableModalCallback: null
    });
    this.setState({findInferenceModalCallbacks: {
        getInferenceSuggestions: this.getNamingInferenceSuggestions,
        getPremiseSuggestions: this.getNamingPremiseSuggestions,
        submit: this.createNamingStep
      }})
  };

  hideFindInferenceModal = () => {
    this.setState({findInferenceModalCallbacks: null})
  };

  getStepInferenceSuggestions = (searchText) => {
    return this.props.apiService.fetchJsonForStep(this.props.path, `suggestInferences?searchText=${searchText}&withConclusion=true`)
  };
  getNamingInferenceSuggestions = (searchText) => {
    return this.props.apiService.fetchJsonForStep(this.props.path, `suggestNamingInferences?searchText=${searchText}`)
  };
  getPremiseSuggestions = (inferenceId) => {
    return this.props.apiService.fetchJsonForStep(this.props.path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=true`)
  };
  getNamingPremiseSuggestions = (inferenceId) => {
    return this.props.apiService.fetchJsonForStep(this.props.path, `suggestNamingPremises?inferenceId=${inferenceId}`)
  };

  proveWithInference = (inferenceId, substitutions) => {
    return this.props.apiService.fetchJsonForStep(this.props.path, "", {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions})
    }).then(this.props.apiService.updateTheorem);
  };

  createNamingStep = (inferenceId, substitutions) => {
    const {boundVariableName: variableName} = this.state;
    return this.props.apiService.fetchJsonForStep(this.props.path, "introduceNaming", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions, variableName})
    }).then(this.props.apiService.updateTheorem);
  };

  introduceDeduction = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "introduceDeduction", {
      method: "POST"
    }).then(this.props.apiService.updateTheorem);
  };

  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists} = this.props;
    let reference = {stepPath: path};
    let scopingStatement = _.find(window.definitions, d => _.includes(d.attributes, "scoping"));
    let deductionStatement = _.find(window.definitions, d => _.includes(d.attributes, "deduction"));

    const boundVariableModal = <BoundVariableModal show={this.isShowingBoundVariableModal()}
                                                   onHide={this.hideBoundVariableModal}
                                                   title={this.state.boundVariableModalTitle}
                                                   label={this.state.boundVariableModalLabel}
                                                   value={this.state.boundVariableName}
                                                   onChange={this.updateBoundVariableName}
                                                   onSave={this.state.boundVariableModalCallback}/>;
    const wrapEditableBoundVariable = (boundVariableContent, boundVariableName, boundVariableIndex, boundVariablePath) =>
      <ClickableText
        onClick={() => this.startUpdatingBoundVariable(boundVariableName, boundVariableIndex, boundVariablePath)}>
        {boundVariableContent}
      </ClickableText>;

    const buttons = (
      <>
        <Button variant="success" size="sm" onClick={this.findInferenceForAssertion}>Find inference</Button>
        <Button variant="success" size="sm" className="ml-1" onClick={this.chooseBoundVariableForNaming}>Name</Button>
        {scopingStatement && step.statement.definition === scopingStatement &&
        <Button variant="success" size="sm" className="ml-1" onClick={this.startIntroducingBoundVariable}>Introduce bound variable</Button>}
        {deductionStatement && step.statement.definition === deductionStatement &&
        <Button variant="success" size="sm" className="ml-1" onClick={this.introduceDeduction}>Introduce deduction</Button>}
      </>
    );
    return <>
      <ProofLine incomplete
                 statement={step.statement}
                 boundVariableLists={boundVariableLists}
                 path={path}
                 buttons={buttons}
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
      {<FindInferenceModal show={this.shouldShowFindInferenceModal()}
                           onHide={this.hideFindInferenceModal}
                           callbacks={this.state.findInferenceModalCallbacks}
                           boundVariableLists={boundVariableLists} />}
    </>
  }
}
