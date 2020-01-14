import _ from "lodash";
import React from "react";
import {Button} from "react-bootstrap";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import EntryContext from "../../../../EntryContext";
import {CopiableExpression, ExpressionComponent} from "../../../../ExpressionComponent";
import InferenceAutosuggest from "./components/InferenceAutosuggest";
import SuggestionDropdownElement from "./components/SuggestionDropdownElement";
import ProofContext from "../../ProofContext";
import BoundVariableLists from "../BoundVariableLists";

export default class ExtractWithPremise extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      autosuggestValue: "",
      facts: [],
      selectedFact: null,
      saving: false
    };
  }
  componentDidMount() {
    const highlightingActions = _.map(this.props.premises, s => { return {reference: s.reference, action: () => {
        this.state.onPremiseSuggestionSelected(s);
        this.context.setHighlightingAction(highlightingActions, [s.reference]);
      }}});
    this.context.setHighlightingAction(highlightingActions);
  }

  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.context.fetchJsonForStep(this.props.path, `suggestFacts?searchText=${value}`)
      .then(factsJson => {
        if (this.state.autosuggestValue === value) {
          this.setState({facts: _.map(factsJson, this.context.parser.parseInference)});
        }
      });
  };
  onSuggestionsClearRequested = () => {
    this.setState({facts: []})
  };
  onSuggestionSelected = (event, {suggestion}) => {
    this.setState({selectedFact: suggestion, selectedBasePremise: null});
  };

  onSave = () => {
    return new Promise((resolve => this.setState({saving: true}, resolve)))
      .then(() => this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "extractWithPremise", {
        method: "POST",
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({
          inferenceId: this.state.selectedFact && this.state.selectedFact.id,
          serializedBasePremiseStatement: this.state.selectedBasePremise && this.state.selectedBasePremise.statement.serialize(),
          serializedHelperPremiseStatement: this.state.selectedHelperPremise.statement.serialize()
        })
      }))
      .then(this.props.onCancel)
      .catch(this.props.onError)
      .then(() => this.setState({saving: false}));
  };

  render() {
    const {availablePremises, title} = this.props;
    const {selectedFact, selectedBasePremise, selectedHelperPremise, saving} = this.state;

    let getSuggestionValue = s => s.name;
    let renderSuggestion = s => <SuggestionDropdownElement
      mainElement={getSuggestionValue(s)}
      hoverElement={<CopiableExpression expression={s.conclusion} />} />;

    return <BoundVariableLists.Consumer>{ boundVariableLists =>
      <EntryContext.Consumer>{ entryContext => <>
        <Form.Group>
          <Form.Label><strong>Extract using premise from fact</strong></Form.Label>
          <InferenceAutosuggest
            key={selectedBasePremise ? selectedBasePremise.serializedReference : ""}
            autoFocus={!selectedBasePremise}
            value={this.state.autosuggestValue}
            onValueChange={this.onAutosuggestChange}
            suggestions={this.state.facts}
            getSuggestionValue={getSuggestionValue}
            renderSuggestion={renderSuggestion}
            readOnly={saving}
            onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
            onSuggestionsClearRequested={this.onSuggestionsClearRequested}
            onSuggestionSelected={this.onSuggestionSelected} />
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Extract using premise from base premise</strong></Form.Label>
          <Form.Control as="select" readOnly={saving} value={selectedBasePremise ? selectedBasePremise.serializedReference : ""} onChange={e => this.setState({selectedBasePremise: _.find(availablePremises, p => p.serializedReference === e.target.value), selectedFact: null })}>
            <option value="" />
            {availablePremises.map(p =>
              <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
                  <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists} entryContext={entryContext} />
                )}}/>
            )}
          </Form.Control>
        </Form.Group>
        {(selectedFact || selectedBasePremise) && <Form.Group>
          <Form.Label><strong>With helper premise</strong></Form.Label>
          <Form.Control as="select" readOnly={saving} value={selectedHelperPremise ? selectedHelperPremise.serializedReference : ""} onChange={e => this.setState({selectedHelperPremise: _.find(availablePremises, p => p.serializedReference === e.target.value)})}>
            <option value="" />
            {availablePremises.map(p =>
              <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
                  <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists} entryContext={entryContext} />
                )}}/>
            )}
          </Form.Control>
        </Form.Group>}
        <Form.Group>
          <Button variant="success" onClick={() => this.onSave()} disabled={saving || !((selectedFact || selectedBasePremise) && selectedHelperPremise)}>{saving ? <span className="fas fa-spin fa-spinner"/>  : "Extract"}</Button>
        </Form.Group>
      </>
      }</EntryContext.Consumer>
    }</BoundVariableLists.Consumer>;
  }
};
