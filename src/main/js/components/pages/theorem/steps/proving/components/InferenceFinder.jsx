import _ from "lodash";
import React from "react";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import DisplayContext from "../../../../../DisplayContext";
import AvailableEntries from "../../../../../AvailableEntries";
import {CopiableExpression, ExpressionComponent} from "../../../../../ExpressionComponent";
import {joinAsList} from "../../../../../helpers/reactFunctions";
import ProofContext from "../../../ProofContext";
import BoundVariableLists from "../../BoundVariableLists";
import ConclusionChooser from "./ConclusionChooser";
import InferenceAutosuggest from "./InferenceAutosuggest";
import SuggestionDropdownElement from "./SuggestionDropdownElement";


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
    return this.setStatePromise({selectedInference})
      .then(() => {
        if (selectedInference.possibleTargets && selectedInference.possibleTargets.length === 1) {
          return this.setSelectedTarget(selectedInference.possibleTargets[0]);
        }
      });
  };
  setSelectedTarget = (selectedTarget) => {
    return this.setStatePromise({selectedTarget});
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
      hoverElement={<AvailableEntries.Consumer>{availableEntries =>
        <DisplayContext.Provider value={DisplayContext.forInferenceSummary(s.inference, availableEntries)}>
          { s.inference.premises.length && <>
            {joinAsList(s.inference.premises.map(p => <CopiableExpression expression={p}/>), false)}
            {" ⊢ "}
          </>}
          <CopiableExpression expression={s.inference.conclusion}/>
        </DisplayContext.Provider>
      }</AvailableEntries.Consumer>} />;

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
      {possibleTargets && possibleTargets.length === 1 &&
      <BoundVariableLists.Consumer>{boundVariableLists =>
        <Form.Group>
          <Form.Label><strong>Target</strong></Form.Label>
          <div><CopiableExpression expression={possibleTargets[0].target} boundVariableLists={[...boundVariableLists, ...possibleTargets[0].additionalBoundVariables]}/></div>
        </Form.Group>
      }</BoundVariableLists.Consumer>
      }
      {possibleTargets && possibleTargets.length > 1 && <BoundVariableLists.Consumer>{boundVariableLists =>
        <AvailableEntries.Consumer>{availableEntries =>
          <DisplayContext.Consumer>{displayContext =>
          <Form.Group>
            <Form.Label><strong>Choose target</strong></Form.Label>
            <Form.Control as="select"
                          value={selectedTarget ? _.indexOf(possibleTargets, selectedTarget) : ""}
                          onChange={e => this.setSelectedTarget(possibleTargets[e.target.value])}
                          readOnly={disabled}>
              <option value=""/>
              {possibleTargets.map(({target, additionalBoundVariables}, index) =>
                <option key={index} value={index} dangerouslySetInnerHTML={{__html: renderToString(<ExpressionComponent expression={target} boundVariableLists={[...boundVariableLists, ...additionalBoundVariables]} availableEntries={availableEntries} displayContext={displayContext}/>)}}/>
              )}
            </Form.Control>
          </Form.Group>
          }</DisplayContext.Consumer>
        }</AvailableEntries.Consumer>
      }</BoundVariableLists.Consumer>}
      {possibleConclusions && <ConclusionChooser possibleConclusions={possibleConclusions}
                                                 conclusionVariableDefinitions={selectedInference.inference.variableDefinitions}
                                                 defaultConclusionStatement={selectedInference.inference.conclusion}
                                                 fetchPossiblePremises={(selectedConclusion) => this.props.fetchPossiblePremises(selectedInference.inference, selectedTarget.wrappingDefinitions, selectedConclusion.extractionDefinition)}
                                                 submit={this.submit}
                                                 disabled={disabled}
                                                 boundVariableListsForPremises={[]}
                                                 boundVariableListsForSubstitutions={selectedTarget ? selectedTarget.additionalBoundVariables : []}/>}
    </div>;
  }
}
