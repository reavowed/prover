import React from "react";
import Button from "react-bootstrap/Button";
import Popover from "react-bootstrap/Popover";
import {FlexRow} from "../FlexRow";
import {BoundVariableModal, FindInferenceModal} from "../Modals";
import {DeleteStepButton} from "./DeleteStepButton";
import {ProofLine} from "./ProofLine";
import {HighlightableExpression} from "../ExpressionComponent";

export class TargetStep extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      showBoundVariableModal: false,
      showFindInferenceModal: false,
      boundVariableName: props.step.statement.boundVariableNames && props.step.statement.boundVariableNames[0] || ""
    };
  }

  showBoundVariableModal = () => {
    this.setState({showBoundVariableModal: true})
  };

  hideBoundVariableModal = () => {
    this.setState({showBoundVariableModal: false})
  };

  updateBoundVariableName = (event) => {
    this.setState({boundVariableName: event.target.value})
  };

  introduceBoundVariable = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "introduceBoundVariable", {
      method: "POST",
      body: this.state.boundVariableName
    }).then(this.props.apiService.updateTheorem);
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

  proveWithInference = (inferenceId, substitutions) => {
    this.props.apiService.fetchJsonForStep(this.props.path, "", {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions})
    }).then(this.props.apiService.updateTheorem);
  };

  render() {
    let {step, path, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let scopingStatement = _.find(window.definitions, d => d.structureType === "scoping");
    let deductionStatement = _.find(window.definitions, d => d.structureType === "deduction");

    const boundVariableModal = <BoundVariableModal show={this.state.showBoundVariableModal}
                                                   onHide={this.hideBoundVariableModal}
                                                   title="Introduce bound variable"
                                                   value={this.state.boundVariableName}
                                                   onChange={this.updateBoundVariableName}
                                                   onSave={this.introduceBoundVariable}/>;

    const popover = (
      <Popover title={<FlexRow><FlexRow.Grow>Statement to prove</FlexRow.Grow><DeleteStepButton path={path} {...otherProps}/></FlexRow>}>
        <Button variant="success" size="sm" onClick={this.showFindInferenceModal}>Find inference</Button>
        {scopingStatement && step.statement.definition === scopingStatement &&
        <Button variant="success" size="sm" className="ml-1" onClick={this.showBoundVariableModal}>Introduce bound variable</Button>}
        {deductionStatement && step.statement.definition === deductionStatement &&
        <Button variant="success" size="sm" className="ml-1" onClick={this.introduceDeduction}>Introduce deduction</Button>}
      </Popover>
    );
    return <>
      <ProofLine incomplete step={step} popover={popover} path={path} {...otherProps}>Then <HighlightableExpression statement={step.statement} references={[...additionalReferences, reference]} {...otherProps}/>.</ProofLine>
      {boundVariableModal}
      {<FindInferenceModal show={this.state.showFindInferenceModal} onHide={this.hideFindInferenceModal} onSubmit={this.proveWithInference} getInferenceSuggestions={this.getInferenceSuggestions} getPremiseSuggestions={this.getPremiseSuggestions} {...otherProps} />}
    </>
  }
}
