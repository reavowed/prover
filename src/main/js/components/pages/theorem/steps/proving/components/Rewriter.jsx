import _ from "lodash";
import React from "react";
import {Button} from "react-bootstrap";
import Form from "react-bootstrap/Form";
import {CopiableExpression} from "../../../../../ExpressionComponent";
import InferenceAutosuggest from "./InferenceAutosuggest";
import SuggestionDropdownElement from "./SuggestionDropdownElement";
import ProofContext from "../../../ProofContext";

export default class Rewriter extends React.Component {
  static contextType = ProofContext;
  constructor(...args) {
    super(...args);
    this.state = {
      currentExpression: this.props.expression,
      selectedInferenceSuggestion: null,
      selectedPremiseSuggestion: null,
      chosenRewrites: [[]],
      saving: false
    };
  }
  componentDidMount() {
    this.loadPremiseSuggestions(this.state.currentExpression, this.getCurrentPaths(), this.onPremiseSelected);
  }
  componentWillUnmount() {
    this.cancelPremiseSuggestions();
  }

  getCurrentPaths = () => _.map(this.state.chosenRewrites[this.state.chosenRewrites.length - 1], r => r.path);

  fetchRewriteSuggestions = (searchText) => {
    const serializedExpression = encodeURIComponent(this.state.currentExpression.serialize());
    const serializedPaths = _.map(this.getCurrentPaths(), p => p.join(".")).join(",");
    return this.context.fetchJsonForStep(
      this.props.path,
      `rewriteSuggestions?searchText=${searchText}&expression=${serializedExpression}&pathsAlreadyRewritten=${serializedPaths}`
    ).then(this.context.parser.parseInferenceRewriteSuggestions)
  };
  onInferenceSelected = (selectedInferenceSuggestion) => {
    this.setState({selectedInferenceSuggestion, selectedPremiseSuggestion: null});
    this.resetPremiseSuggestions();
  };

  onPremiseSelected = (selectedPremiseSuggestion) => {
    this.setState({selectedInferenceSuggestion: null, selectedPremiseSuggestion});
  };

  onExpressionClickedForInference = (path, replacementExpression, inferenceId, reverse) => {
    this.addRewrite({path, inferenceId, reverse}, replacementExpression);
  };
  onExpressionClickedForPremise = (path, replacementExpression, serializedPremiseStatement, reverse) => {
    this.addRewrite({path, serializedPremiseStatement, reverse}, replacementExpression);
  };

  loadPremiseSuggestions = (expression, pathsAlreadyRewritten, onPremiseSuggestionSelected) => {
    this.context.fetchJsonForStep(this.props.path, `rewritePremiseSuggestions?expression=${encodeURIComponent(expression.serialize())}&pathsAlreadyRewritten=${_.map(pathsAlreadyRewritten, p => p.join(".")).join(",")}`)
      .then((suggestions) => {
        this.setState({premiseSuggestions: this.context.parser.parsePremiseRewriteSuggestions(suggestions), onPremiseSuggestionSelected}, () => this.resetPremiseSuggestions());
      });
  };
  resetPremiseSuggestions = () => {
    const highlightingActions = _.map(this.state.premiseSuggestions, s => { return {reference: s.reference, action: () => {
        this.state.onPremiseSuggestionSelected(s);
        this.context.setHighlightingAction(highlightingActions, [s.reference]);
      }}});
    this.context.setHighlightingAction(highlightingActions);
  };
  cancelPremiseSuggestions = () => {
    this.setState({premiseSuggestions: [], onPremiseSuggestionSelected: () => {}});
    this.context.clearHighlightingAction();
  };

  addRewrite = (rewrite, replacementExpression) => {
    let {chosenRewrites} = this.state;
    chosenRewrites = [...chosenRewrites.slice(0, chosenRewrites.length - 1), [...chosenRewrites[chosenRewrites.length - 1], rewrite]];
    this.setState({
      currentExpression: this.state.currentExpression.replaceAtPath(rewrite.path, replacementExpression),
      chosenRewrites
    });
  };

  save = () => {
    return new Promise((resolve => this.setState({saving: true}, resolve)))
      .then(() => this.props.onSave(this.state.chosenRewrites))
      .catch(() => {})
      .then(() => this.setState({saving: false}));
  };

  again = () => {
    this.setState({
      chosenRewrites: [...this.state.chosenRewrites, []],
      inferenceSuggestions: [],
      selectedInferenceSuggestion: null,
      selectedPremiseSuggestion: null
    })
  };

  render() {
    const {title} = this.props;
    const {currentExpression, selectedInferenceSuggestion, selectedPremiseSuggestion, chosenRewrites, saving} = this.state;
    const currentPaths = this.getCurrentPaths();

    const actionHighlights = selectedInferenceSuggestion ?
      _.chain(selectedInferenceSuggestion.rewriteSuggestions)
        .filter(s => !_.some(currentPaths, path => _.startsWith(s.path, path)))
        .map(s => { return {path: s.path, action: () => this.onExpressionClickedForInference(s.path, s.result, selectedInferenceSuggestion.inference.id, selectedInferenceSuggestion.reverse) }})
        .value() :
      selectedPremiseSuggestion ?
        _.chain(selectedPremiseSuggestion.rewriteSuggestions)
          .filter(s => !_.some(currentPaths, path => _.startsWith(s.path, path)))
          .map(s => { return {path: s.path, action: () => this.onExpressionClickedForPremise(s.path, s.result, selectedPremiseSuggestion.statement.serialize(), s.reverse) }})
          .value() :

      [];
    const staticHighlights = _.map(currentPaths, path => { return {path}});

    let getSuggestionValue = s => s.inference.name;
    let renderSuggestion = s => <SuggestionDropdownElement
      mainElement={getSuggestionValue(s)}
      hoverElement={<><CopiableExpression expression={s.source} /> -> <CopiableExpression expression={s.result} /></>} />;

    const saveDisabled = saving || chosenRewrites.length === 0 || chosenRewrites[chosenRewrites.length - 1].length === 0;

    return <>
      <Form.Group>
        <Form.Label><strong>{title}</strong></Form.Label>
        <div>
          <CopiableExpression expression={currentExpression} actionHighlights={actionHighlights} staticHighlights={staticHighlights} />
        </div>
      </Form.Group>
      <Form.Group>
        <Form.Label><strong>Inference</strong></Form.Label>
        <InferenceAutosuggest
          key={chosenRewrites.length} // Force reset when Again button is clicked
          autofocus
          fetchSuggestions={this.fetchRewriteSuggestions}
          setSelectedSuggestion={this.onInferenceSelected}
          getSuggestionValue={getSuggestionValue}
          renderSuggestion={renderSuggestion}
          readOnly={saving} />
      </Form.Group>
      <Form.Group>
        <Button variant="success" onClick={this.save} disabled={saveDisabled}>{saving ? <span className="fas fa-spin fa-spinner"/>  : "Save"}</Button>
        <Button variant="primary" className="ml-1" onClick={this.again} disabled={saveDisabled}>Again</Button>
      </Form.Group>
    </>
  }
};
