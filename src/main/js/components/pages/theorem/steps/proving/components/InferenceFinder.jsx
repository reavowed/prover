import _ from "lodash";
import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {Parser} from "../../../../../../Parser";
import EntryContext from "../../../../../EntryContext";
import {CopiableExpression, ExpressionComponent} from "../../../../../ExpressionComponent";
import InputWithShorthandReplacement from "../../../../../helpers/InputWithShorthandReplacement";
import ConclusionChooser from "./ConclusionChooser";
import InferenceAutosuggest from "./InferenceAutosuggest";
import {InferenceSummary} from "../../../../../InferenceSummary";
import SuggestionDropdownElement from "./SuggestionDropdownElement";
import BoundVariableLists from "../../BoundVariableLists";
import ProofContext from "../../../ProofContext";


export class InferenceFinder extends React.Component {
  static contextType = ProofContext;
  constructor(...args) {
    super(...args);
    this.ref = React.createRef();
    this.autoSuggestRef = React.createRef();
    this.state = {
      saving: false,
      premiseSuggestions: null,
      selectedInference: null,
      selectedConclusion: null,
      selectedPremises: []
    };
  }
  componentDidMount() {
    if (this.props.focusOnMount) {
      this.autoSuggestRef.current.input.focus();
      this.ref.current.scrollIntoView();
    }
  }
  fetchPossibleInferences = (value) => {
    return this.props.getInferenceSuggestions(value)
      .then(suggestionsJson => this.context.parser.parsePossibleInferences(suggestionsJson))
  };
  setSelectedInference = (selectedInference) => {
    this.setState({selectedInference});
  };
  submit = (selectedConclusion, selectedSubstitutionValues) => {
    return this.submitWithSelectedValues(this.state.selectedInference, selectedConclusion, selectedSubstitutionValues);
  };
  submitWithSelectedValues = (selectedInference, selectedConclusion, substitutionValues) => {
    const promise = new Promise((resolve) => this.setState({saving: true}, resolve))
      .then(() => this.props.onSaving && this.props.onSaving(true))
      .then(() => this.props.submit(selectedInference, selectedConclusion, substitutionValues));
    promise.catch(() => {})
      .then(() => this.setState({saving: false}))
      .then(() => this.props.onSaving && this.props.onSaving(false));
    return promise;
  };

  render() {
    const disabled = this.props.disabled || this.state.saving;

    const {title, autofocus} = this.props;
    const {selectedInference} = this.state;

    let getSuggestionValue = s => s.inference.name;
    let renderSuggestion = s => <SuggestionDropdownElement
      mainElement={getSuggestionValue(s)}
      hoverElement={<CopiableExpression expression={s.inference.conclusion} />} />;

    return <div ref={this.ref}>
      <Form.Group>
        <Form.Label><strong>{title}</strong></Form.Label>
        <InferenceAutosuggest
          autofocus={autofocus}
          fetchSuggestions={this.fetchPossibleInferences}
          setSelectedSuggestion={this.setSelectedInference}
          getSuggestionValue={getSuggestionValue}
          renderSuggestion={renderSuggestion}
          readOnly={disabled} />
      </Form.Group>
      {this.state.selectedInference && <ConclusionChooser possibleConclusions={selectedInference.possibleConclusions}
                                                          defaultConclusionStatement={selectedInference.inference.conclusion}
                                                          submit={this.submit}
                                                          disabled={disabled}
                                                          boundVariableListsForPremises={[]}/>}
    </div>;
  }
}
