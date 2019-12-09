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
      chosenRewrites: [[]]
    };
  }
  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.getSuggestions(value, this.state.currentExpression, _.map(this.state.chosenRewrites[this.state.chosenRewrites.length - 1], r => r.path))
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
  onExpressionClicked = (path, replacementExpression, inferenceId, reverse) => {
    let {chosenRewrites} = this.state;
    chosenRewrites = [...chosenRewrites.slice(0, chosenRewrites.length - 1), [...chosenRewrites[chosenRewrites.length - 1], {path, inferenceId, reverse}]];
    this.setState({
      currentExpression: this.state.currentExpression.replaceAtPath(path, replacementExpression),
      chosenRewrites
    });
  };
  again = () => {
    this.setState({
      chosenRewrites: [...this.state.chosenRewrites, []],
      inferenceSuggestions: [],
      selectedSuggestion: null
    })
  };

  render() {
    const {title, boundVariableLists, onSave} = this.props;
    const {currentExpression, selectedSuggestion, chosenRewrites} = this.state;
    const currentPaths =  _.map(chosenRewrites[chosenRewrites.length - 1], r => r.path);

    const actionHighlights = selectedSuggestion ?
      _.chain(selectedSuggestion.rewriteSuggestions)
        .filter(s => !_.some(currentPaths, path => _.startsWith(s.path, path)))
        .map(s => { return {path: s.path, action: () => this.onExpressionClicked(s.path, s.result, selectedSuggestion.inference.id, selectedSuggestion.reverse) }})
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
