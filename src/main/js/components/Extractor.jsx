import _ from "lodash";
import React from "react";
import {Button} from "react-bootstrap";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {connect} from "react-redux";
import {Parser} from "../Parser";
import {CopiableExpression} from "./ExpressionComponent";
import InferenceAutosuggest from "./InferenceAutosuggest";
import SuggestionDropdownElement from "./SuggestionDropdownElement";
import ProofContext from "./theorem/ProofContext";
import {FetchJsonForStep, FetchJsonForStepAndUpdate, SetHighlightingAction} from "./theorem/TheoremStore";

export default connect()(class Extractor extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      autosuggestValue: "",
      facts: [],
      selectedFact: null
    };
  }
  componentDidMount() {
    const highlightingActions = _.map(this.props.premises, s => { return {reference: s.reference, action: () => {
        this.state.onPremiseSuggestionSelected(s);
        this.props.dispatch(SetHighlightingAction(highlightingActions, [s.reference]));
      }}});
    this.props.dispatch(SetHighlightingAction(highlightingActions));
  }

  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestFacts?searchText=${value}`))
      .then(factsJson => {
        if (this.state.autosuggestValue === value) {
          this.setState({facts: _.map(factsJson, Parser.parseInference)});
        }
      });
  };
  onSuggestionsClearRequested = () => {
    this.setState({facts: []})
  };
  onSuggestionSelected = (event, {suggestion}) => {
    this.setState({selectedFact: suggestion, selectedPremise: null});
  };

  onSave = () => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "extractWithPremise", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId: this.state.selectedFact.id, serializedPremiseStatement: this.state.selectedPremise.statement.serialize()})
    }));
  };

  render() {
    const {availablePremises, boundVariableLists, title} = this.props;
    const {selectedPremise, selectedFact} = this.state;

    let getSuggestionValue = s => s.name;
    let renderSuggestion = s => <SuggestionDropdownElement
      mainElement={getSuggestionValue(s)}
      hoverElement={<CopiableExpression expression={s.conclusion} boundVariableLists={[]} />} />;

    return <>
      <Form.Group>
        <Form.Label><strong>{title} using fact</strong></Form.Label>
        <InferenceAutosuggest
          autoFocus
          value={this.state.autosuggestValue}
          onValueChange={this.onAutosuggestChange}
          suggestions={this.state.facts}
          getSuggestionValue={getSuggestionValue}
          renderSuggestion={renderSuggestion}
          onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
          onSuggestionsClearRequested={this.onSuggestionsClearRequested}
          onSuggestionSelected={this.onSuggestionSelected} />
      </Form.Group>
      <Form.Group>
        <Form.Label><strong>And premise</strong></Form.Label>
        <Form.Control as="select" value={selectedPremise ? selectedPremise.serializedReference : ""} onChange={e => this.setState({selectedPremise: _.find(availablePremises, p => p.serializedReference === e.target.value)})}>
          <option value="" />
          {availablePremises.map(p =>
            <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
                <CopiableExpression expression={p.statement} boundVariableLists={boundVariableLists} />
              )}}/>
          )}
        </Form.Control>
      </Form.Group>
      <Form.Group>
        <Button variant="success" onClick={() => this.onSave()} disabled={!selectedFact || !selectedPremise}>Extract</Button>
      </Form.Group>
    </>;
  }
});
