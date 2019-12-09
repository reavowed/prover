import _ from "lodash";
import React from "react";
import {Col, Row} from "react-bootstrap";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import {connect} from "react-redux";
import {CopiableExpression} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {InferenceFinder} from "../InferenceFinder";
import Rewriter from "../Rewriter";
import {FetchJsonForStep, FetchJsonForStepAndUpdate, SetHighlightingAction} from "../theorem/TheoremStore";
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
  rewriteAutomatically = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "rewriteAutomatically", { method: "POST" }));
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
    this.cancelImmediateNamingPremises();
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
  getSubstitutionSuggestionsForStep = (inferenceId, selectedPremises) => {
    return this.getSubstitutionSuggestions(inferenceId, selectedPremises, true);
  };
  getSubstitutionSuggestionsForPremise = (inferenceId, selectedPremises) => {
    return this.getSubstitutionSuggestions(inferenceId, selectedPremises, false);
  };

  getSubstitutionSuggestions = (inferenceId, selectedPremises, withConclusion) => {
    return this.props.dispatch(FetchJsonForStep(
      this.context.proofIndex,
      this.props.path,
      `suggestSubstitutions`,
      {
        method: "POST",
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({
          inferenceId,
          serializedPremises: _.mapValues(selectedPremises, p => p.serialize()),
          withConclusion
        })
      }
    ));
  }

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
    }))
      .then(this.setProvingType(null));
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
    }))
      .then(this.setProvingType(null));
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
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "transitiveTarget", {
      method: "POST",
      body: this.state.targetStatement
    }));
  };

  setProvingType = (provingType) => {
    this.setState({
      activeProvingType: provingType,
      targetStatement: '',
      namingVariableName: ''
    });
    if (provingType === 'naming') {
      this.setState({immediateNamingActive: true});
      this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, "suggestImmediateNamingPremises"))
        .then(this.handleImmediateNamingPremises);
    } else {
      this.cancelImmediateNamingPremises();
    }
  };

  handleImmediateNamingPremises = (paths) => {
    if (this.state.immediateNamingActive) {
      this.props.dispatch(SetHighlightingAction(paths, this.handleImmediateNamingPremiseSelected));
    }
  };
  handleImmediateNamingPremiseSelected = (premise) => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "introduceNamingFromPremise", { method: "POST", body: premise }))
      .then(() => {
        this.props.dispatch(SetHighlightingAction([], null));
        this.context.callOnStep([...this.props.path, 0], "startProving")
      });
  };
  cancelImmediateNamingPremises = () => {
    this.setState({immediateNamingActive: false});
    this.props.dispatch(SetHighlightingAction([], null));
  };

  getRewriteSuggestions = (searchText, expression, pathsAlreadyRewritten) => {
    return this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `rewriteSuggestions?searchText=${searchText}&expression=${encodeURIComponent(expression.serialize())}&pathsAlreadyRewritten=${_.map(pathsAlreadyRewritten, p => p.join(".")).join(",")}`));
  };
  rewrite = (rewrites) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "rewrite", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(rewrites)
    }));
  };
  rewriteLeft = (rewrites) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "rewriteLeft", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(rewrites)
    }));
  };
  rewriteRight = (rewrites) => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "rewriteRight", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(rewrites)
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
            <CopiableExpression expression={step.statement} boundVariableLists={boundVariableLists}/>
          </h5>
          <div>
            {!transitive &&
              <Row className="mb-1">
                <Col xs={2} className="text-right">
                  Insert before
                </Col>
                <Col xs={10}>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('premise')}>Add premise</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('naming')}>Name</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'target', targetStatement: ''})}>Add target</Button>
                </Col>
              </Row>
            }
            <Row className="mb-1">
              <Col xs={2} className="text-right">
                Prove directly
              </Col>
              <Col xs={10}>
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('inference')}>Prove with inference</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('rewrite')}>Rewrite</Button>
                {!transitive && scopingStatement && step.statement.definition === scopingStatement &&
                <Button size="sm" className="ml-1" onClick={this.introduceBoundVariable}>Introduce bound variable</Button>}
                {!transitive && deductionStatement && step.statement.definition === deductionStatement &&
                <Button size="sm" className="ml-1" onClick={this.introduceDeduction}>Introduce deduction</Button>}
              </Col>
            </Row>
            <Row className="mb-1">
              <Col xs={2} className="text-right">
                Automatic
              </Col>
              <Col xs={10}>
                <Button size="sm" className="ml-1" onClick={this.extract}>Extract</Button>
                <Button size="sm" className="ml-1" onClick={this.rearrange}>Rearrange</Button>
                <Button size="sm" className="ml-1" onClick={this.rewriteAutomatically}>Rewrite</Button>
              </Col>
            </Row>
            {transitive &&
              <Row className="mb-1">
                <Col xs={2} className="text-right">
                  Add transitive
                </Col>
                <Col xs={10}>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('addFromLeft')}>Add expression from left</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('addFromRight')}>Add expression from right</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('rewriteLeft')}>Rewrite left</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('rewriteRight')}>Rewrite right</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('transitiveTarget')}>Add target</Button>
                </Col>
              </Row>
            }
          </div>
          {activeProvingType === 'premise' && <InferenceFinder title='Select Inference for Premise'
                                                               getInferenceSuggestions={this.getInferenceSuggestionsForPremise}
                                                               getPremiseSuggestions={this.getPremiseSuggestionsForPremise}
                                                               getSubstitutionSuggestions={this.getSubstitutionSuggestionsForPremise}
                                                               boundVariableLists={boundVariableLists}
                                                               submit={this.addPremise}
                                                               autofocus/>}
          {activeProvingType === 'inference' && <InferenceFinder title='Select Inference'
                                                                 getInferenceSuggestions={this.getInferenceSuggestionsForStep}
                                                                 getPremiseSuggestions={this.getPremiseSuggestionsForStep}
                                                                 getSubstitutionSuggestions={this.getSubstitutionSuggestionsForStep}
                                                                 boundVariableLists={boundVariableLists}
                                                                 submit={this.proveWithInference}
                                                                 autofocus/>}
          {activeProvingType === 'rewrite' && <Rewriter
            title="Rewriting"
            expression={step.statement}
            boundVariableLists={boundVariableLists}
            getSuggestions={this.getRewriteSuggestions}
            onSave={this.rewrite}
          />}
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
                                onChange={(e) => {
                                  const [targetStatement, callback] = Parser.replaceShorthands(e);
                                  this.setState({targetStatement}, callback);
                                }}/>
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
                                                                   autofocus/>}
          {activeProvingType === 'addFromRight' && <InferenceFinder title='Select Inference to Add from Right'
                                                                    getInferenceSuggestions={this.getInferenceSuggestionsForRight}
                                                                    getPremiseSuggestions={this.getPremiseSuggestionsForRight}
                                                                    boundVariableLists={boundVariableLists}
                                                                    submit={this.addFromRight}
                                                                    autofocus/>}
          {activeProvingType === 'rewriteLeft' && <Rewriter
            title="Rewriting Left"
            expression={step.statement.components[0]}
            boundVariableLists={boundVariableLists}
            getSuggestions={this.getRewriteSuggestions}
            onSave={this.rewriteLeft}
          />}
          {activeProvingType === 'rewriteRight' && <Rewriter
            title="Rewriting Right"
            expression={step.statement.components[1]}
            boundVariableLists={boundVariableLists}
            getSuggestions={this.getRewriteSuggestions}
            onSave={this.rewriteRight}
          />}
          {activeProvingType === 'transitiveTarget' && <>
            <Form.Group>
              <Form.Label><strong>Transitive Target</strong></Form.Label>
              <FlexRow>
                <FlexRow.Grow>
                  <Form.Control type="text"
                                autoFocus
                                value={this.state.targetStatement}
                                onChange={(e) => {
                                  const [targetStatement, callback] = Parser.replaceShorthands(e);
                                  this.setState({targetStatement}, callback);
                                }}/>
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
