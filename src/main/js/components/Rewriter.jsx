import React from "react";
import {Button} from "react-bootstrap";
import Form from "react-bootstrap/Form";
import {Expression} from "../models/Expression";
import {Parser} from "../Parser";
import {CopiableExpression} from "./ExpressionComponent";
import InferenceAutosuggest from "./InferenceAutosuggest";

export default class extends React.Component {
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
    this.props.loadPremiseSuggestions(this.state.currentExpression, this.getCurrentPaths(), this.onPremiseSelected);
  }
  componentWillUnmount() {
    this.props.cancelPremiseSuggestions();
  }

  getCurrentPaths = () => _.map(this.state.chosenRewrites[this.state.chosenRewrites.length - 1], r => r.path);

  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.getSuggestions(value, this.state.currentExpression, this.getCurrentPaths())
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
    this.props.resetPremiseSuggestions();
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
          .map(s => { return {path: s.path, action: () => this.onExpressionClickedForPremise(s.path, s.result, selectedPremiseSuggestion.statement.serialize(), selectedPremiseSuggestion.reverse) }})
          .value() :

      [];
    const staticHighlights = _.map(currentPaths, path => { return {path}});

    class SuggestionDropdownElement extends React.Component {
      constructor(props) {
        super(props);
        this.state = {showConclusion: false};
      }
      render() {
        const {suggestion} = this.props;
        return <div onMouseEnter={() => this.setState({showConclusion: true})} onMouseLeave={() => this.setState({showConclusion: false})}>
          {getSuggestionValue(suggestion)} {this.state.showConclusion && <> (<CopiableExpression expression={suggestion.source} boundVariableLists={[]} /> -> <CopiableExpression expression={suggestion.result} boundVariableLists={[]} />)</>}
        </div>
      }
    }

    let getSuggestionValue = s => s.inference.name;
    let renderSuggestion = s => <SuggestionDropdownElement suggestion={s}/>;

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
}
