import _ from "lodash";
import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import {ExpressionComponent} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {InferenceFinder} from "../Modals";
import {ProofLine} from "./ProofLine";
import {Parser} from "../../Parser";

export class TargetStepProofLine extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      proving: false,
      activeProvingType: null
    };
    this.props.theoremContext.registerStep(this, this.props.path);
  }
  componentWillUnmount() {
    this.props.theoremContext.unregisterStep(this.props.path);
  }

  introduceBoundVariable = () => {
    this.props.theoremContext.fetchJsonForStep(this.props.path, "introduceBoundVariable", {
      method: "POST"
    })
      .then(this.props.theoremContext.updateTheorem)
      .then(() => this.props.theoremContext.callOnStep([...this.props.path, 0], "startProving"));
  };
  introduceDeduction = () => {
    this.props.theoremContext.fetchJsonForStep(this.props.path, "introduceDeduction", {
      method: "POST"
    })
      .then(this.props.theoremContext.updateTheorem)
      .then(() => this.props.theoremContext.callOnStep([...this.props.path, 0], "startProving"));
  };
  extract = () => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, "extract", { method: "POST" })
      .then(this.props.theoremContext.updateTheorem);
  };
  rearrange = () => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, "rearrange", { method: "POST" })
      .then(this.props.theoremContext.updateTheorem);
  };
  rewrite = () => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, "rewrite", { method: "POST" })
      .then(this.props.theoremContext.updateTheorem);
  };

  startProving = () => {
    this.setState({
      proving: true,
      activeProvingType: null
    });
  };

  stopProving = () => {
    this.setState({
      proving: false,
      activeProvingType: null
    });
  };

  getInferenceSuggestionsForStep = (searchText) => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, `suggestInferences?searchText=${searchText}`)
  };
  getInferenceSuggestionsForPremise = (searchText) => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, `suggestInferencesForPremise?searchText=${searchText}`)
  };
  getInferenceSuggestionsForNaming = (searchText) => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, `suggestNamingInferences?searchText=${searchText}`)
  };
  getPremiseSuggestionsForStep = (inferenceId) => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=true`)
  };
  getPremiseSuggestionsForPremise = (inferenceId) => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=false`)
  };
  getPremiseSuggestionsForNaming = (inferenceId) => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, `suggestNamingPremises?inferenceId=${inferenceId}`)
  };
  proveWithInference = (inferenceId, substitutions, rewriteInferenceId) => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, "", {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions, rewriteInferenceId})
    })
      .then(this.props.theoremContext.updateTheorem)
      .then(this.stopProving);
  };
  addPremise = (inferenceId, substitutions, rewriteInferenceId) => {
    return this.props.theoremContext.fetchJsonForStep(this.props.path, "assertion", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions, rewriteInferenceId})
    }).then(this.props.theoremContext.updateTheorem)
      .then(this.startProving);
  };
  createNamingStep = (inferenceId, substitutions) => {
    const {namingVariableName: variableName} = this.state;
    return this.props.theoremContext.fetchJsonForStep(this.props.path, "introduceNaming", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions, variableName})
    }).then(this.props.theoremContext.updateTheorem)
      .then(this.startProving);
  };
  addTarget = () => {
    this.props.theoremContext.fetchJsonForStep(this.props.path, "target", {
      method: "POST",
      body: this.state.targetStatement
    })
      .then(this.props.theoremContext.updateTheorem)
      .then(this.startProving);
  };

  render() {
    let {step, path, additionalReferences, theoremContext, boundVariableLists, children} = this.props;
    let {proving, activeProvingType} = this.state;
    let scopingStatement = _.find(window.definitions, d => _.includes(d.attributes, "scoping"));
    let deductionStatement = _.find(window.definitions, d => _.includes(d.attributes, "deduction"));
    return <>
      {proving ?
        <div className="card" style={{margin: ".5rem", padding: ".5rem .75rem"}}>
          <Button size="sm" variant="danger" className="float-left" onClick={() => this.setState({proving: false})} style={{position: "absolute"}}><i className="fas fa-times"/></Button>
          <h5 className="text-center">
            <ExpressionComponent expression={step.statement} boundVariableLists={boundVariableLists}/>
          </h5>
          <div className="text-center">
            <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'inference'})}>Prove with inference</Button>
            <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'premise'})}>Add premise</Button>
            <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'naming', namingVariableName: ''})}>Name</Button>
            <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'target', targetStatement: ''})}>Add target</Button>
            <Button size="sm" className="ml-1" onClick={this.extract}>Extract</Button>
            <Button size="sm" className="ml-1" onClick={this.rearrange}>Rearrange</Button>
            <Button size="sm" className="ml-1" onClick={this.rewrite}>Rewrite</Button>
            {scopingStatement && step.statement.definition === scopingStatement &&
            <Button size="sm" className="ml-1" onClick={this.introduceBoundVariable}>Introduce bound variable</Button>}
            {deductionStatement && step.statement.definition === deductionStatement &&
            <Button size="sm" className="ml-1" onClick={this.introduceDeduction}>Introduce deduction</Button>}
          </div>
          {activeProvingType === 'premise' && <InferenceFinder title='Select Inference for Premise'
                                                               getInferenceSuggestions={this.getInferenceSuggestionsForPremise}
                                                               getPremiseSuggestions={this.getPremiseSuggestionsForPremise}
                                                               boundVariableLists={boundVariableLists}
                                                               submit={this.addPremise}
                                                               focusOnMount/>}
          {activeProvingType === 'inference' && <InferenceFinder title='Select Inference'
                                                                 getInferenceSuggestions={this.getInferenceSuggestionsForStep}
                                                                 getPremiseSuggestions={this.getPremiseSuggestionsForStep}
                                                                 boundVariableLists={boundVariableLists}
                                                                 submit={this.proveWithInference}
                                                                 focusOnMount/>}
          {activeProvingType === 'naming' && <>
            <Form.Group>
              <Form.Label><strong>Variable name</strong></Form.Label>
              <Form.Control type="text"
                            value={this.state.namingVariableName}
                            onChange={(e) => this.setState({namingVariableName: e.target.value})}/>
            </Form.Group>
            <InferenceFinder title='Select Inference for Naming'
                             getInferenceSuggestions={this.getInferenceSuggestionsForNaming}
                             getPremiseSuggestions={this.getPremiseSuggestionsForNaming}
                             boundVariableLists={boundVariableLists}
                             submit={this.createNamingStep}/>
          </>}
          {activeProvingType === 'target' && <>
            <Form.Group>
              <Form.Label><strong>Target</strong></Form.Label>
              <FlexRow>
                <FlexRow.Grow>
                  <Form.Control type="text"
                                value={this.state.targetStatement}
                                onChange={(e) => this.setState({targetStatement: Parser.replaceShorthands(e.target.value)})}/>
                </FlexRow.Grow>
                <Button size="sm" className="ml-1" onClick={this.addTarget}>Add</Button>
              </FlexRow>
            </Form.Group>
          </>}
        </div> :
        <ProofLine incomplete
                   editableBoundVariable
                   path={path}
                   additionalReferences={additionalReferences}
                   buttons={<Button variant="danger" size="sm" className="pt-0 pb-0" onClick={this.startProving}>Prove</Button>}
                   theoremContext={theoremContext}>
          {children}
        </ProofLine>}
    </>
  }
}

export class TargetStep extends React.Component {
  render() {
    const {step, path, boundVariableLists, theoremContext} = this.props;
    return <TargetStepProofLine {...this.props}>
      <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                  prefix="Then"
                                                  statement={step.statement}
                                                  path={path}
                                                  boundVariableLists={boundVariableLists}
                                                  theoremContext={theoremContext} />
    </TargetStepProofLine>
  }
}
