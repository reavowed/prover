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
      selectedTarget: null,
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
  setSelectedTarget = (selectedTarget) => {
    this.setState({selectedTarget});
  };
  submit = (selectedConclusion, selectedSubstitutionValues, premiseStatements, conclusionStatement) => {
    return this.submitWithSelectedValues(this.state.selectedInference, this.state.selectedTarget, selectedConclusion, selectedSubstitutionValues, premiseStatements, conclusionStatement);
  };
  submitWithSelectedValues = (selectedInference, selectedTarget, selectedConclusion, substitutionValues, premiseStatements, conclusionStatement) => {
    const promise = new Promise((resolve) => this.setState({saving: true}, resolve))
      .then(() => this.props.onSaving && this.props.onSaving(true))
      .then(() => this.props.submit(selectedInference, selectedTarget, selectedConclusion, substitutionValues, premiseStatements, conclusionStatement));
    promise.catch(() => {})
      .then(() => this.setState({saving: false}))
      .then(() => this.props.onSaving && this.props.onSaving(false));
    return promise;
  };

  render() {
    const disabled = this.props.disabled || this.state.saving;

    const {title, autofocus} = this.props;
    const {selectedInference, selectedTarget} = this.state;

    let getSuggestionValue = s => s.inference.name;
    let renderSuggestion = s => <SuggestionDropdownElement
      mainElement={getSuggestionValue(s)}
      hoverElement={<CopiableExpression expression={s.inference.conclusion} />} />;

    const possibleTargets = selectedInference && selectedInference.possibleTargets;
    const possibleConclusions = (selectedInference && selectedInference.possibleConclusions) || (selectedTarget && selectedTarget.possibleConclusions);

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
      {possibleTargets && possibleTargets.length > 1 && <BoundVariableLists.Consumer>{boundVariableLists =>
        <EntryContext.Consumer>{entryContext =>
          <Form.Group>
            <Form.Label><strong>Choose target</strong></Form.Label>
            <Form.Control as="select"
                          value={selectedTarget ? _.indexOf(possibleTargets, selectedTarget) : ""}
                          onChange={e => this.setSelectedTarget(possibleTargets[e.target.value])}
                          readOnly={disabled}>
              <option value=""/>
              {possibleTargets.map(({target, additionalBoundVariables}, index) =>
                <option key={index} value={index} dangerouslySetInnerHTML={{__html: renderToString(<ExpressionComponent expression={target} boundVariableLists={[...boundVariableLists, ...additionalBoundVariables]} entryContext={entryContext}/>)}}/>
              )}
            </Form.Control>
          </Form.Group>
        }</EntryContext.Consumer>
      }</BoundVariableLists.Consumer>}
      {possibleConclusions && <ConclusionChooser possibleConclusions={possibleConclusions}
                                                 defaultConclusionStatement={selectedInference.inference.conclusion}
                                                 submit={this.submit}
                                                 disabled={disabled}
                                                 boundVariableListsForPremises={[]}
                                                 boundVariableListsForSubstitutions={selectedTarget ? selectedTarget.additionalBoundVariables : []}/>}
    </div>;
  }
}
