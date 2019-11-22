import _ from "lodash";
import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import {connect} from "react-redux";
import {ExpressionComponent} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {InferenceFinder} from "../InferenceFinder";
import {FetchJsonForStep, FetchJsonForStepAndUpdate} from "../theorem/TheoremStore";
import ProofLine from "./ProofLine";
import {Parser} from "../../Parser";
import ProofContext from "../theorem/ProofContext";

export const TargetStepProofLine = connect()(class extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      proving: false,
      activeProvingType: null
    };
  }
  componentDidMount() {
    this.context.registerStep(this, this.props.path);
  }
  componentWillUnmount() {
    this.context.unregisterStep(this.props.path);
  }

  introduceBoundVariable = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "introduceBoundVariable", {
      method: "POST"
    }))
      .then(() => this.context.callOnStep([...this.props.path, 0], "startProving"));
  };
  introduceDeduction = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "introduceDeduction", {
      method: "POST"
    }))
      .then(() => this.context.callOnStep([...this.props.path, 0], "startProving"));
  };
  extract = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "extract", { method: "POST" }));
  };
  rearrange = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "rearrange", { method: "POST" }));
  };
  rewrite = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "rewrite", { method: "POST" }));
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
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestInferences?searchText=${searchText}`));
  };
  getInferenceSuggestionsForPremise = (searchText) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestInferencesForPremise?searchText=${searchText}`));
  };
  getInferenceSuggestionsForNaming = (searchText) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestNamingInferences?searchText=${searchText}`));
  };
  getPremiseSuggestionsForStep = (inferenceId) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=true`));
  };
  getPremiseSuggestionsForPremise = (inferenceId) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=false`));
  };
  getPremiseSuggestionsForNaming = (inferenceId) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestNamingPremises?inferenceId=${inferenceId}`));
  };
  proveWithInference = (suggestion, substitutions) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "", {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      })
    }));
  };
  addPremise = (suggestion, substitutions) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "assertion", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId: suggestion.inference.id, substitutions})
    }));
  };
  createNamingStep = (suggestion, substitutions) => {
    const {namingVariableName: variableName} = this.state;
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "introduceNaming", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId: suggestion.inference.id, substitutions, variableName})
    }))
      .then(() => this.context.callOnStep([...this.props.path, 0], "startProving"));
  };
  addTarget = () => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "target", {
      method: "POST",
      body: this.state.targetStatement
    }));
  };

  getInferenceSuggestionsForLeft = (searchText) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestInferencesForTransitivityFromLeft?searchText=${searchText}`));
  };
  getPremiseSuggestionsForLeft = (inferenceId) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestPremisesForTransitivityFromLeft?inferenceId=${inferenceId}`));
  };
  addFromLeft = (suggestion, substitutions) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "transitivityFromLeft", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      })
    }));
  };

  getInferenceSuggestionsForRight = (searchText) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestInferencesForTransitivityFromRight?searchText=${searchText}`));
  };
  getPremiseSuggestionsForRight = (inferenceId) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestPremisesForTransitivityFromRight?inferenceId=${inferenceId}`));
  };
  addFromRight = (suggestion, substitutions) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "transitivityFromRight", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      })
    }));
  };

  addTransitiveTarget = () => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, "transitiveTarget", {
      method: "POST",
      body: this.state.targetStatement
    }));
  };

  render() {
    let {step, path, additionalReferences, boundVariableLists, children, transitive} = this.props;
    let {proving, activeProvingType} = this.state;
    let scopingStatement = _.find(window.definitions, d => _.includes(d.attributes, "scoping"));
    let deductionStatement = _.find(window.definitions, d => _.includes(d.attributes, "deduction"));
    return <>
      {proving ?
        <div className="card" style={{margin: ".5rem", padding: ".5rem .75rem"}}>
          <Button size="sm" variant="danger" className="float-left" onClick={this.stopProving} style={{position: "absolute"}}><i className="fas fa-times"/></Button>
          <h5 className="text-center">
            <ExpressionComponent expression={step.statement} boundVariableLists={boundVariableLists}/>
          </h5>
          <div className="text-center">
            {!transitive &&
              <>
                <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'inference'})}>Prove with inference</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'premise'})}>Add premise</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'naming', namingVariableName: ''})}>Name</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'target', targetStatement: ''})}>Add target</Button>
              </>
            }
            {transitive &&
            <>
              <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'addFromLeft'})}>Add expression from left</Button>
              <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'addFromRight'})}>Add expression from right</Button>
              <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'transitiveTarget', targetStatement: ''})}>Add target</Button>
            </>
            }
            <Button size="sm" className="ml-1" onClick={this.extract}>Extract</Button>
            <Button size="sm" className="ml-1" onClick={this.rearrange}>Rearrange</Button>
            <Button size="sm" className="ml-1" onClick={this.rewrite}>Rewrite</Button>
            {!transitive && scopingStatement && step.statement.definition === scopingStatement &&
            <Button size="sm" className="ml-1" onClick={this.introduceBoundVariable}>Introduce bound variable</Button>}
            {!transitive && deductionStatement && step.statement.definition === deductionStatement &&
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
                            autoFocus
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
                                autoFocus
                                value={this.state.targetStatement}
                                onChange={(e) => this.setState({targetStatement: Parser.replaceShorthands(e.target.value)})}/>
                </FlexRow.Grow>
                <Button size="sm" className="ml-1" onClick={this.addTarget}>Add</Button>
              </FlexRow>
            </Form.Group>
          </>}
          {activeProvingType === 'addFromLeft' && <InferenceFinder title='Select Inference to Add from Left'
                                                                   getInferenceSuggestions={this.getInferenceSuggestionsForLeft}
                                                                   getPremiseSuggestions={this.getPremiseSuggestionsForLeft}
                                                                   boundVariableLists={boundVariableLists}
                                                                   submit={this.addFromLeft}
                                                                   focusOnMount/>}
          {activeProvingType === 'addFromRight' && <InferenceFinder title='Select Inference to Add from Right'
                                                                   getInferenceSuggestions={this.getInferenceSuggestionsForRight}
                                                                   getPremiseSuggestions={this.getPremiseSuggestionsForRight}
                                                                   boundVariableLists={boundVariableLists}
                                                                   submit={this.addFromRight}
                                                                   focusOnMount/>}
          {activeProvingType === 'transitiveTarget' && <>
            <Form.Group>
              <Form.Label><strong>Transitive Target</strong></Form.Label>
              <FlexRow>
                <FlexRow.Grow>
                  <Form.Control type="text"
                                autoFocus
                                value={this.state.targetStatement}
                                onChange={(e) => this.setState({targetStatement: Parser.replaceShorthands(e.target.value)})}/>
                </FlexRow.Grow>
                <Button size="sm" className="ml-1" onClick={this.addTransitiveTarget}>Add</Button>
              </FlexRow>
            </Form.Group>
          </>}
        </div> :
        <ProofLine incomplete
                   editableBoundVariable
                   path={path}
                   additionalReferences={additionalReferences}
                   buttons={<Button variant="danger" size="sm" className="pt-0 pb-0" onClick={this.startProving}>Prove</Button>}>
          {children}
        </ProofLine>}
    </>
  }
});

export class TargetStep extends React.Component {
  render() {
    const {step, path, boundVariableLists} = this.props;
    return <TargetStepProofLine {...this.props}>
      <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                  prefix="Then"
                                                  statement={step.statement}
                                                  path={path}
                                                  boundVariableLists={boundVariableLists} />
    </TargetStepProofLine>
  }
}
