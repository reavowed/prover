import _ from "lodash";
import React from "react";
import {Col, Row} from "react-bootstrap";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {matchTemplate} from "../../../../models/Expression";
import {Parser} from "../../../../Parser";
import EntryContext from "../../../EntryContext";
import {CopiableExpression, ExpressionComponent} from "../../../ExpressionComponent";
import Extractor from "./proving/Extractor";
import {FlexRow} from "../../../FlexRow";
import InputWithShorthandReplacement from "../../../helpers/InputWithShorthandReplacement";
import {InferenceFinder} from "./proving/InferenceFinder";
import PremiseOrFactChooser from "./proving/PremiseOrFactChooser";
import Rewriter from "./proving/Rewriter";
import ProofContext from "../ProofContext";
import BoundVariableLists from "./BoundVariableLists";
import ProofLine from "./components/ProofLine";
import Step from "./Step";

export class TargetStepProofLine extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      proving: false,
      activeProvingType: null,
      availablePremises: []
    };
  }
  componentDidMount() {
    this.context.registerStep(this, this.props.path);
  }
  componentWillUnmount() {
    this.context.unregisterStep(this.props.path);
  }
  onUpdate() {
    this.context.fetchJsonForStep(this.props.path, "premises")
      .then(premiseJson => this.setState({availablePremises: _.map(premiseJson, Parser.parsePremise)}));
  }

  introduceBoundVariable = () => {
    this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "introduceBoundVariable", {
      method: "POST"
    })
      .then(() => this.context.callOnStep([...this.props.path, 0], "startProving"));
  };
  introduceDeduction = () => {
    this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "introduceDeduction", {
      method: "POST"
    })
      .then(() => this.context.callOnStep([...this.props.path, 0], "startProving"));
  };
  extractAutomatically = () => {
    this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "extractAutomatically", { method: "POST" });
  };
  rearrange = () => {
    this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "rearrange", { method: "POST" });
  };
  rewriteAutomatically = () => {
    this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "rewriteAutomatically", { method: "POST" });
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
    return this.context.fetchJsonForStep(this.props.path, `suggestInferences?searchText=${searchText}`);
  };
  getInferenceSuggestionsForPremise = (searchText) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestInferencesForPremise?searchText=${searchText}`);
  };
  getInferenceSuggestionsForNaming = (searchText) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestNamingInferences?searchText=${searchText}`);
  };
  getPremiseSuggestionsForStep = (inferenceId) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=true`);
  };
  getPremiseSuggestionsForPremise = (inferenceId) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=false`);
  };
  getPremiseSuggestionsForNaming = (inferenceId) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestNamingPremises?inferenceId=${inferenceId}`);
  };
  getSubstitutionSuggestionsForStep = (inferenceId, selectedPremises) => {
    return this.getSubstitutionSuggestions(inferenceId, selectedPremises, true);
  };
  getSubstitutionSuggestionsForPremise = (inferenceId, selectedPremises) => {
    return this.getSubstitutionSuggestions(inferenceId, selectedPremises, false);
  };

  getSubstitutionSuggestions = (inferenceId, selectedPremises, withConclusion) => {
    return this.context.fetchJsonForStep(
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
    );
  };

  proveWithInference = (suggestion, substitutions) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "", {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      })
    });
  };
  addPremise = (suggestion, substitutions) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "assertion", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId: suggestion.inference.id, substitutions})
    })
      .then(() => this.setProvingType(null));
  };
  createNamingStep = (suggestion, substitutions) => {
    const {namingVariableName: variableName} = this.state;
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "introduceNaming", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId: suggestion.inference.id, substitutions, variableName})
    })
      .then(() => this.context.callOnStep([...this.props.path, 0], "startProving"));
  };
  addTarget = () => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "target", {
      method: "POST",
      body: this.state.targetStatement
    })
      .then(() => this.setProvingType(null));
  };

  getInferenceSuggestionsForLeft = (searchText) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestInferencesForTransitivityFromLeft?searchText=${searchText}`);
  };
  getPremiseSuggestionsForLeft = (inferenceId) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestPremisesForTransitivityFromLeft?inferenceId=${inferenceId}`);
  };
  addFromLeft = (suggestion, substitutions) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "transitivityFromLeft", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      })
    });
  };

  getInferenceSuggestionsForRight = (searchText) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestInferencesForTransitivityFromRight?searchText=${searchText}`);
  };
  getPremiseSuggestionsForRight = (inferenceId) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestPremisesForTransitivityFromRight?inferenceId=${inferenceId}`);
  };
  addFromRight = (suggestion, substitutions) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "transitivityFromRight", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      })
    });
  };

  addTransitiveTarget = () => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "transitiveTarget", {
      method: "POST",
      body: this.state.targetStatement
    });
  };

  setProvingType = (provingType) => {
    this.context.clearHighlightingAction();
    this.setState({
      activeProvingType: provingType,
      targetStatement: '',
      namingVariableName: ''
    });
    if (provingType === 'naming') {
      this.setState({immediateNamingActive: true});
      this.context.fetchJsonForStep(this.props.path, "suggestImmediateNamingPremises")
        .then(premiseJson => _.map(premiseJson, Parser.parsePremise))
        .then(this.handleImmediateNamingPremises);
    } else {
      this.cancelImmediateNamingPremises();
    }
    if (provingType === 'target') {
      this.setState({targetStatement: ''});
    }
    if (provingType === 'rewritePremise') {
      this.setState({premiseToRewrite: ""});
    }
    if (provingType === 'premiseLeft') {
      this.context.fetchJsonForStep(this.props.path, "suggestTransitivityFromPremiseLeft")
        .then(premiseJson => _.map(premiseJson, Parser.parsePremise))
        .then(premises => {
          const highlightingActions = _.map(premises, p => {return {reference: p.referencedLine, action: () => this.premiseLeft(p)}});
          this.context.setHighlightingAction(highlightingActions);
        });
    }
    if (provingType === 'premiseRight') {
      this.context.fetchJsonForStep(this.props.path, "suggestTransitivityFromPremiseRight")
        .then(premiseJson => _.map(premiseJson, Parser.parsePremise))
        .then(premises => {
          const highlightingActions = _.map(premises, p => {return {reference: p.referencedLine, action: () => this.premiseRight(p)}});
          this.context.setHighlightingAction(highlightingActions);
        });
    }
  };

  handleImmediateNamingPremises = (premises) => {
    if (this.state.immediateNamingActive) {
      const highlightingActions = _.map(premises, p => {return {reference: p.referencedLine, action: () => this.handleImmediateNamingPremiseSelected(p.statement)}});
      this.context.setHighlightingAction(highlightingActions);
    }
  };
  handleImmediateNamingPremiseSelected = (premise) => {
    this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "introduceNamingFromPremise", { method: "POST", body: premise.serialize() })
      .then(() => {
        this.context.clearHighlightingAction();
        this.context.callOnStep([...this.props.path, 0], "startProving")
      });
  };
  cancelImmediateNamingPremises = () => {
    this.setState({immediateNamingActive: false});
    this.context.clearHighlightingAction();
  };

  rewrite = (rewrites) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "rewrite", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(rewrites)
    });
  };
  rewritePremise = (rewrites) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "rewritePremise", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(rewrites)
    }).then(() => this.setProvingType(null));
  };
  rewriteLeft = (rewrites) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "rewriteLeft", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(rewrites)
    }).then(() => this.setProvingType(null));
  };
  rewriteRight = (rewrites) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "rewriteRight", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(rewrites)
    }).then(() => this.setProvingType(null));
  };
  premiseLeft = (premise) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "premiseLeft", {
      method: "POST",
      body: premise.statement.serialize()
    }).then(() => this.context.clearHighlightingAction())
      .then(() => this.setProvingType(null));
  };
  premiseRight = (premise) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "premiseRight", {
      method: "POST",
      body: premise.statement.serialize()
    }).then(() => this.context.clearHighlightingAction())
      .then(() => this.setProvingType(null));
  };

  extractWithPremise = (premise) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "extract", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({serializedPremiseStatement: premise.statement.serialize()})
    }).then(() => this.setProvingType(null));
  };
  extractWithFact = (fact) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "extract", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId: fact.id})
    }).then(() => this.setProvingType(null));
  };


  render() {
    const {step, path, additionalReferences, children, transitive} = this.props;
    const {proving, activeProvingType} = this.state;

    return <EntryContext.Consumer>{entryContext => {
      const scopingStatement = _.find(entryContext.definitions, d => _.includes(d.attributes, "scoping"));
      const deductionStatement = _.find(entryContext.definitions, d => _.includes(d.attributes, "deduction"));
      const binaryRelation = _.find(_.reverse(entryContext.binaryRelations.slice()), x => matchTemplate(x.template, step.statement, [], []));
      return proving ?
        <div className="card" style={{margin: ".5rem", padding: ".5rem .75rem"}}>
          <Button size="sm" variant="danger" className="float-left" onClick={this.stopProving} style={{position: "absolute"}}><i className="fas fa-times"/></Button>
          <h5 className="text-center">
            <CopiableExpression expression={step.statement} />
          </h5>
          <div>
            <Row className="mb-1">
              <Col xs={2} className="text-right">
                Prove directly
              </Col>
              <Col xs={10}>
                {!transitive && scopingStatement && step.statement.definition === scopingStatement &&
                <Button size="sm" className="ml-1" onClick={this.introduceBoundVariable}>Introduce bound variable</Button>}
                {!transitive && deductionStatement && step.statement.definition === deductionStatement &&
                <Button size="sm" className="ml-1" onClick={this.introduceDeduction}>Introduce deduction</Button>}
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('inference')}>Prove with inference</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('rewrite')}>Rewrite</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('extract')}>Extract</Button>
              </Col>
            </Row>
            {!transitive &&
            <Row className="mb-1">
              <Col xs={2} className="text-right">
                Insert before
              </Col>
              <Col xs={10}>
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('premise')}>Add premise</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('naming')}>Name</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('rewritePremise')}>Rewrite premise</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setProvingType('extractPremise')}>Extract with premise</Button>
                <Button size="sm" className="ml-1" onClick={() => this.setState({activeProvingType: 'target', targetStatement: ''})}>Add target</Button>
              </Col>
            </Row>
            }
            <Row className="mb-1">
              <Col xs={2} className="text-right">
                Automatic
              </Col>
              <Col xs={10}>
                <Button size="sm" className="ml-1" onClick={this.extractAutomatically}>Extract</Button>
                <Button size="sm" className="ml-1" onClick={this.rearrange}>Rearrange</Button>
                <Button size="sm" className="ml-1" onClick={this.rewriteAutomatically}>Rewrite</Button>
              </Col>
            </Row>
            {binaryRelation &&
              <Row className="mb-1">
                <Col xs={2} className="text-right">
                  Add transitive
                </Col>
                <Col xs={10}>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('addFromLeft')}>From left</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('addFromRight')}>From right</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('rewriteLeft')}>Rewrite left</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('rewriteRight')}>Rewrite right</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('premiseLeft')}>Premise left</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('premiseRight')}>Premise right</Button>
                  <Button size="sm" className="ml-1" onClick={() => this.setProvingType('transitiveTarget')}>Insert target</Button>
                </Col>
              </Row>
            }
          </div>
          {activeProvingType === 'premise' && <InferenceFinder title='Select Inference for Premise'
                                                               getInferenceSuggestions={this.getInferenceSuggestionsForPremise}
                                                               getPremiseSuggestions={this.getPremiseSuggestionsForPremise}
                                                               getSubstitutionSuggestions={this.getSubstitutionSuggestionsForPremise}
                                                               submit={this.addPremise}
                                                               autofocus/>}
          {activeProvingType === 'inference' && <InferenceFinder title='Select Inference'
                                                                 getInferenceSuggestions={this.getInferenceSuggestionsForStep}
                                                                 getPremiseSuggestions={this.getPremiseSuggestionsForStep}
                                                                 getSubstitutionSuggestions={this.getSubstitutionSuggestionsForStep}
                                                                 submit={this.proveWithInference}
                                                                 autofocus/>}
          {activeProvingType === 'rewrite' && <Rewriter
            title="Rewriting"
            expression={step.statement}
            path={path}
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
                             submit={this.createNamingStep}/>
          </>}
          {activeProvingType === 'target' && <>
            <Form.Group>
              <Form.Label><strong>Target</strong></Form.Label>
              <FlexRow>
                <FlexRow.Grow>
                  <InputWithShorthandReplacement autoFocus
                                                 value={this.state.targetStatement}
                                                 onChange={(targetStatement, callback) => this.setState({targetStatement}, callback)}/>
                </FlexRow.Grow>
                <Button size="sm" className="ml-1" onClick={this.addTarget}>Add</Button>
              </FlexRow>
            </Form.Group>
          </>}
          {activeProvingType === 'addFromLeft' && <InferenceFinder title='Select Inference to Add from Left'
                                                                   getInferenceSuggestions={this.getInferenceSuggestionsForLeft}
                                                                   getPremiseSuggestions={this.getPremiseSuggestionsForLeft}
                                                                   submit={this.addFromLeft}
                                                                   autofocus/>}
          {activeProvingType === 'addFromRight' && <InferenceFinder title='Select Inference to Add from Right'
                                                                    getInferenceSuggestions={this.getInferenceSuggestionsForRight}
                                                                    getPremiseSuggestions={this.getPremiseSuggestionsForRight}
                                                                    submit={this.addFromRight}
                                                                    autofocus/>}
          {activeProvingType === 'rewriteLeft' && <Rewriter
            title="Rewriting Left"
            expression={step.statement.components[0]}
            path={path}
            onSave={this.rewriteLeft}
          />}
          {activeProvingType === 'rewriteRight' && <Rewriter
            title="Rewriting Right"
            expression={step.statement.components[1]}
            path={path}
            onSave={this.rewriteRight}
          />}
          {activeProvingType === 'rewritePremise' && <BoundVariableLists.Consumer>{boundVariableLists =>
            <Form.Group>
              <Form.Label><strong>Choose premise</strong></Form.Label>
              <Form.Control as="select" autoFocus value={this.state.premiseToRewrite && this.state.premiseToRewrite.serializedReference} onChange={e => this.setState({premiseToRewrite: _.find(this.state.availablePremises, p => p.serializedReference === e.target.value).statement})}>
                <option value="" />
                {this.state.availablePremises.map(p =>
                  <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
                      <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists} />
                    )}}/>
                )}
              </Form.Control>
            </Form.Group>
          }</BoundVariableLists.Consumer>}
          {this.state.premiseToRewrite && <Rewriter
            title="Rewriting Premise"
            expression={this.state.premiseToRewrite}
            path={path}
            onSave={rewrites => this.rewritePremise({serializedPremise: this.state.premiseToRewrite.serialize(), rewrites})}
          />}
          {activeProvingType === 'extract' && <PremiseOrFactChooser
            title="Extract from"
            availablePremises={this.state.availablePremises}
            path={path}
            onPremiseSelected={this.extractWithPremise}
            onFactSelected={this.extractWithFact}
          />}
          {activeProvingType === 'extractPremise' && <Extractor
            title="Extract using premise"
            availablePremises={this.state.availablePremises}
            path={path}
            onSave={() => this.setProvingType(null)}
          />}
          {activeProvingType === 'transitiveTarget' && <>
            <Form.Group>
              <Form.Label><strong>Transitive Target</strong></Form.Label>
              <FlexRow>
                <FlexRow.Grow>
                  <InputWithShorthandReplacement autoFocus
                                                 value={this.state.targetStatement}
                                                 onChange={(targetStatement, callback) => this.setState({targetStatement}, callback)}/>
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
        </ProofLine>;
    }}</EntryContext.Consumer>
  }
}

export class TargetStep extends React.Component {
  render() {
    const {step, path, additionalReferences} = this.props;
    return <Step.WithoutSubsteps>
      <TargetStepProofLine {...this.props}>
        <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                    prefix="Then"
                                                    statement={step.statement}
                                                    path={path}
                                                    additionalReferences={additionalReferences} />
      </TargetStepProofLine>
    </Step.WithoutSubsteps>
  }
}
