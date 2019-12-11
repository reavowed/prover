import _ from "lodash";
import React from "react";
import {Button} from "react-bootstrap";
import Form from "react-bootstrap/Form";
import {connect} from "react-redux";
import {Parser} from "../Parser";
import {CopiableExpression} from "./ExpressionComponent";
import InferenceAutosuggest from "./InferenceAutosuggest";
import SuggestionDropdownElement from "./SuggestionDropdownElement";
import ProofContext from "./theorem/ProofContext";
import {ClearHighlightingAction, FetchJsonForStep, SetHighlightingAction} from "./theorem/TheoremStore";

export default connect()(class Rewriter extends React.Component {
  static contextType = ProofContext;
  constructor(...args) {
    super(...args);
    this.state = {
      autosuggestValue: "",
      currentExpression: this.props.expression,
      inferenceSuggestions: [],
      selectedInferenceSuggestion: null,
      selectedPremiseSuggestion: null,
      chosenRewrites: [[]]
    };
  }
  componentDidMount() {
    this.loadPremiseSuggestions(this.state.currentExpression, this.getCurrentPaths(), this.onPremiseSelected);
  }
  componentWillUnmount() {
    this.cancelPremiseSuggestions();
  }

  getCurrentPaths = () => _.map(this.state.chosenRewrites[this.state.chosenRewrites.length - 1], r => r.path);

  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `rewriteSuggestions?searchText=${value}&expression=${encodeURIComponent(this.state.currentExpression.serialize())}&pathsAlreadyRewritten=${_.map(this.getCurrentPaths(), p => p.join(".")).join(",")}`))
      .then(suggestionsJson => {
        if (this.state.autosuggestValue === value) {
          this.setState({inferenceSuggestions: Parser.parseInferenceRewriteSuggestions(suggestionsJson)})
        }
      })
  };
  onSuggestionsClearRequested = () => {
    this.setState({inferenceSuggestions: []})
  };
  onSuggestionSelected = (event, {suggestion}) => {
    this.setState({selectedInferenceSuggestion: suggestion, selectedPremiseSuggestion: null});
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
    this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `rewritePremiseSuggestions?expression=${encodeURIComponent(expression.serialize())}&pathsAlreadyRewritten=${_.map(pathsAlreadyRewritten, p => p.join(".")).join(",")}`))
      .then((suggestions) => {
        this.setState({premiseSuggestions: Parser.parsePremiseRewriteSuggestions(suggestions), onPremiseSuggestionSelected}, () => this.resetPremiseSuggestions());
      });
  };
  resetPremiseSuggestions = () => {
    const highlightingActions = _.map(this.state.premiseSuggestions, s => { return {reference: s.reference, action: () => {
        this.state.onPremiseSuggestionSelected(s);
        this.props.dispatch(SetHighlightingAction(highlightingActions, [s.reference]));
      }}});
    this.props.dispatch(SetHighlightingAction(highlightingActions));
  };
  cancelPremiseSuggestions = () => {
    this.setState({premiseSuggestions: [], onPremiseSuggestionSelected: () => {}});
    this.props.dispatch(ClearHighlightingAction());
  };

  addRewrite = (rewrite, replacementExpression) => {
    let {chosenRewrites} = this.state;
    chosenRewrites = [...chosenRewrites.slice(0, chosenRewrites.length - 1), [...chosenRewrites[chosenRewrites.length - 1], rewrite]];
    this.setState({
      currentExpression: this.state.currentExpression.replaceAtPath(rewrite.path, replacementExpression),
      chosenRewrites
    });
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
    const {title, boundVariableLists, onSave} = this.props;
    const {currentExpression, selectedInferenceSuggestion, selectedPremiseSuggestion, chosenRewrites} = this.state;
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
      hoverElement={<><CopiableExpression expression={s.source} boundVariableLists={[]} /> -> <CopiableExpression expression={s.result} boundVariableLists={[]} /></>} />;

    return <>
      <Form.Group>
        <Form.Label><strong>{title}</strong></Form.Label>
        <div>
          <CopiableExpression expression={currentExpression} boundVariableLists={boundVariableLists} actionHighlights={actionHighlights} staticHighlights={staticHighlights} />
        </div>
      </Form.Group>
      <Form.Group>
        <Form.Label><strong>Inference</strong></Form.Label>
        <InferenceAutosuggest
          key={chosenRewrites.length} // Force reset when Again button is clicked
          autofocus
          value={this.state.autosuggestValue}
          onValueChange={this.onAutosuggestChange}
          suggestions={this.state.inferenceSuggestions}
          getSuggestionValue={getSuggestionValue}
          renderSuggestion={renderSuggestion}
          onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
          onSuggestionsClearRequested={this.onSuggestionsClearRequested}
          onSuggestionSelected={this.onSuggestionSelected} />
      </Form.Group>
      <Form.Group>
        <Button variant="success" onClick={() => onSave(chosenRewrites)} disabled={chosenRewrites.length === 0 || chosenRewrites[chosenRewrites.length - 1].length === 0}>Save</Button>
        <Button variant="primary" onClick={this.again} disabled={chosenRewrites.length === 0 || chosenRewrites[chosenRewrites.length - 1].length === 0}>Again</Button>
      </Form.Group>
    </>
  }
});
