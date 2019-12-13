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
    this.setState({selectedFact: suggestion, selectedBasePremise: null});
  };

  onSave = () => {
    return this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "extractWithPremise", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: this.state.selectedFact && this.state.selectedFact.id,
        serializedBasePremiseStatement: this.state.selectedBasePremise.statement && this.state.selectedBasePremise.statement.serialize(),
        serializedHelperPremiseStatement: this.state.selectedHelperPremise.statement.serialize()
      })
    })).then(() => this.props.onSave());
  };

  render() {
    const {availablePremises, boundVariableLists, title} = this.props;
    const { selectedFact, selectedBasePremise, selectedHelperPremise } = this.state;

    let getSuggestionValue = s => s.name;
    let renderSuggestion = s => <SuggestionDropdownElement
      mainElement={getSuggestionValue(s)}
      hoverElement={<CopiableExpression expression={s.conclusion} boundVariableLists={[]} />} />;

    return <>
      <Form.Group>
        <Form.Label><strong>{title} from fact</strong></Form.Label>
        <InferenceAutosuggest
          key={selectedBasePremise ? selectedBasePremise.serializedReference : ""}
          autoFocus={!selectedBasePremise}
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
        <Form.Label><strong>{title} from base premise</strong></Form.Label>
        <Form.Control as="select" value={selectedBasePremise ? selectedBasePremise.serializedReference : ""} onChange={e => this.setState({selectedBasePremise: _.find(availablePremises, p => p.serializedReference === e.target.value), selectedFact: null })}>
          <option value="" />
          {availablePremises.map(p =>
            <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
                <CopiableExpression expression={p.statement} boundVariableLists={boundVariableLists} />
              )}}/>
          )}
        </Form.Control>
      </Form.Group>
      {(selectedFact || selectedBasePremise) && <Form.Group>
        <Form.Label><strong>With helper premise</strong></Form.Label>
        <Form.Control as="select" value={selectedHelperPremise ? selectedHelperPremise.serializedReference : ""} onChange={e => this.setState({selectedHelperPremise: _.find(availablePremises, p => p.serializedReference === e.target.value)})}>
          <option value="" />
          {availablePremises.map(p =>
            <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
                <CopiableExpression expression={p.statement} boundVariableLists={boundVariableLists} />
              )}}/>
          )}
        </Form.Control>
      </Form.Group>}
      <Form.Group>
        <Button variant="success" onClick={() => this.onSave()} disabled={!((selectedFact || selectedBasePremise) && selectedHelperPremise)}>Extract</Button>
      </Form.Group>
    </>;
  }
});
