import React from "react";
import {Button} from "react-bootstrap";
import Form from "react-bootstrap/Form";
import {Parser} from "../Parser";
import {CopiableExpression} from "./ExpressionComponent";
import InferenceAutosuggest from "./InferenceAutosuggest";
import {FetchJsonForStepAndUpdate} from "./theorem/TheoremStore";

export default class extends React.Component {
  constructor(...args) {
    super(...args);
    this.state = {
      autosuggestValue: "",
      currentExpression: this.props.expression,
      inferenceSuggestions: [],
      selectedSuggestion: null,
      chosenRewrites: []
    };
  }
  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.getSuggestions(value, this.props.expression, _.map(this.state.chosenRewrites, r => r.path))
      .then(suggestionsJson => {
        if (this.state.autosuggestValue === value) {
          this.setState({inferenceSuggestions: Parser.parseRewriteSuggestions(suggestionsJson)})
        }
      })
  };
  onSuggestionsClearRequested = () => {
    this.setState({inferenceSuggestions: []})
  };
  onSuggestionSelected = (event, {suggestion}) => {
    this.setState({selectedSuggestion: suggestion});
  };
  onExpressionClicked = (path, replacementExpression, inferenceId, direction) => {
    this.setState({
      currentExpression: this.state.currentExpression.replaceAtPath(path, replacementExpression),
      chosenRewrites: [...this.state.chosenRewrites, {path, inferenceId, direction}]
    });
  };

  render() {
    const {title, boundVariableLists, onSave} = this.props;
    const {currentExpression, selectedSuggestion, chosenRewrites} = this.state;

    const actionHighlights = selectedSuggestion ?
      _.chain(selectedSuggestion.rewriteSuggestions)
        .filter(s => !_.some(chosenRewrites, r => _.startsWith(s.path, r.path)))
        .map(s => { return {path: s.path, action: () => this.onExpressionClicked(s.path, s.result, selectedSuggestion.inference.id, s.direction) }})
        .value() :
      [];
    const staticHighlights = _.map(chosenRewrites, r => { return {path: r.path}});

    let getSuggestionValue = s => s.inference.name;

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
          autofocus
          value={this.state.autosuggestValue}
          onValueChange={this.onAutosuggestChange}
          suggestions={this.state.inferenceSuggestions}
          getSuggestionValue={getSuggestionValue}
          onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
          onSuggestionsClearRequested={this.onSuggestionsClearRequested}
          onSuggestionSelected={this.onSuggestionSelected} />
      </Form.Group>
      <Form.Group>
        <Button variant="success" onClick={() => onSave(chosenRewrites)} disabled={chosenRewrites.length === 0}>Save</Button>
      </Form.Group>
    </>
  }
}
