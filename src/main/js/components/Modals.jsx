import React from "react";
import Autosuggest from "react-autosuggest";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import {renderToString} from "react-dom/server";
import styled from "styled-components";
import {Parser} from "../Parser";
import {Expression} from "./Expression";
import {InferenceSummary} from "./InferenceSummary";

const DropdownContainer = styled.div`
  .react-autosuggest__suggestions-container--open & {
    background-color: #fff;
    background-clip: padding-box;
    border: 1px solid rgba(0,0,0,.15);
    border-radius: .25rem;
    ul {
      margin: 0;
      padding: 0;
    }
    li {
      list-style-type: none;
      overflow: hidden;
    }
  }
`;

const AllSubstitutionTypes = ["statements", "terms", "predicates", "functions"];

function buildSubstitutionMap(requiredSubstitutions, f) {
  return _.fromPairs(AllSubstitutionTypes.map(type => {
    const requiredNames = requiredSubstitutions[type];
    const namesToValues = _.fromPairs(_.map(requiredNames, name => [name, f(type, name)]));
    return [type, namesToValues];
  }));
}

export class FindInferenceModal extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      isLoading: false,
      autosuggestValue: "",
      inferenceSuggestions: [],
      premiseSuggestions: null,
      selectedInferenceSuggestion: null,
      selectedPremiseSuggestions: []
    };
  }
  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.getInferenceSuggestions(value)
      .then(response => response.json())
      .then(suggestionsJson => this.setState({inferenceSuggestions: Parser.parseInferenceSuggestions(suggestionsJson)}))
  };
  onSuggestionsClearRequested = () => {
    this.setState({
      inferenceSuggestions: []
    });
  };
  onSuggestionSelected = (event, {suggestion}) => {
    const forcedSubstitutionValues = this.getAllForcedSubstitutionValues([suggestion.substitutions], suggestion.requiredSubstitutions);
    const selectedSubstitutionValues = buildSubstitutionMap(suggestion.requiredSubstitutions, (type, name) => forcedSubstitutionValues[type][name] ? forcedSubstitutionValues[type][name].serialize() : "");
    this.setState({
      selectedInferenceSuggestion: suggestion,
      premiseSuggestions: null,
      selectedSubstitutionValues
    });
    this.props.getPremiseSuggestions(suggestion.inference.id)
      .then(response => response.json())
      .then(suggestionsJson => this.setState({
        premiseSuggestions: Parser.parsePremiseSuggestions(suggestionsJson),
        selectedPremiseSuggestions: suggestion.inference.premises.map(() => ["", null])
      }))
  };

  getAllForcedSubstitutionValues = (possibleSubstitutionsLists, requiredSubstitutions) => {
    const selectableSubstitutionValues = this.getAllSelectableSubstitutionValues(possibleSubstitutionsLists, requiredSubstitutions);
    return buildSubstitutionMap(requiredSubstitutions, (type, name) =>
      selectableSubstitutionValues[type][name] && selectableSubstitutionValues[type][name].length === 1 && selectableSubstitutionValues[type][name][0]
    );
  };
  getAllSelectableSubstitutionValues = (possibleSubstitutionsLists, requiredSubstitutions) => {
    return buildSubstitutionMap(requiredSubstitutions, (type, name) => {
      const allowedValuesLists = _.map(possibleSubstitutionsLists, substitutionsList =>
        _.uniqBy(substitutionsList.map(s => s[type][name]), x => !x || x.serialize())
      );
      const allowAnything = _.every(allowedValuesLists, allowedValues => _.some(allowedValues, _.isUndefined));
      if (allowAnything) {
        return null;
      }
      const listsWithRestrictions = _.filter(allowedValuesLists, allowedValues => !_.some(allowedValues, _.isUndefined));
      return _.intersectionBy(...listsWithRestrictions, x => x.serialize());
    });
  };

  filterBySelectedSubstitutionValues = (substitutionsLists, requiredSubstitutions, selectedSubstitutionValues) => {
    return _.map(substitutionsLists, substitutionsList =>
      _.filter(substitutionsList, s =>
        _.every(AllSubstitutionTypes, type =>
          _.every(requiredSubstitutions[type], name => {
            return selectedSubstitutionValues[type][name] === '' ||
              !s[type][name] ||
              selectedSubstitutionValues[type][name] === s[type][name].serialize();
          })
        )
      )
    );
  };
  getPossibleSubstitutionsLists = (selectedPremiseSuggestions) => {
    return this.filterBySelectedSubstitutionValues(
      [this.state.selectedInferenceSuggestion.substitutions, ..._.filter(_.map(selectedPremiseSuggestions, s => s[1]))],
      this.state.selectedInferenceSuggestion.requiredSubstitutions,
      this.state.selectedSubstitutionValues
    );
  };

  getValidSubstitutionValues = (type, name) => {
    const selectedSubstitutionValues = _.cloneDeep(this.state.selectedSubstitutionValues);
    selectedSubstitutionValues[type][name] = "";
    const compatibleSubstitutions = this.filterBySelectedSubstitutionValues(
      this.getPossibleSubstitutionsLists(this.state.selectedPremiseSuggestions),
      this.state.selectedInferenceSuggestion.requiredSubstitutions,
      selectedSubstitutionValues);
    return this.getAllSelectableSubstitutionValues(compatibleSubstitutions, this.state.selectedInferenceSuggestion.requiredSubstitutions)[type][name];
  };

  setSelectedSubstitutionValue = (type, name, value) => {
    const selectedSubstitutionValues = _.cloneDeep(this.state.selectedSubstitutionValues);
    selectedSubstitutionValues[type][name] = value;
    this.updateForcedSubstitutionValues(selectedSubstitutionValues, this.state.selectedPremiseSuggestions);
    this.setState({selectedSubstitutionValues});
  };
  setSelectedPremiseSuggestion = (premiseIndex, suggestionIndex) => {
    const selectedPremiseSuggestions = _.cloneDeep(this.state.selectedPremiseSuggestions);
    selectedPremiseSuggestions[premiseIndex] = [suggestionIndex, suggestionIndex !== "" ? this.state.premiseSuggestions[premiseIndex][parseInt(suggestionIndex)].substitutions : null];
    this.updateForcedSubstitutionValues(this.state.selectedSubstitutionValues, selectedPremiseSuggestions);
    this.setState({selectedPremiseSuggestions})
  };
  updateForcedSubstitutionValues = (selectedSubstitutionValues, selectedPremiseSuggestions) => {
    const compatibleSubstitutions = this.filterBySelectedSubstitutionValues(
      this.getPossibleSubstitutionsLists(selectedPremiseSuggestions),
      this.state.selectedInferenceSuggestion.requiredSubstitutions,
      selectedSubstitutionValues);
    const forcedSubstitutionValues = this.getAllForcedSubstitutionValues(
      compatibleSubstitutions,
      this.state.selectedInferenceSuggestion.requiredSubstitutions,
      selectedPremiseSuggestions);

    _.each(AllSubstitutionTypes, type => {
      _.each(this.state.selectedInferenceSuggestion.requiredSubstitutions[type], name => {
        if (forcedSubstitutionValues[type][name]) {
          selectedSubstitutionValues[type][name] = forcedSubstitutionValues[type][name].serialize();
        }
      });
    });
  };

  readyToSubmit() {
    return this.state.selectedInferenceSuggestion && _.chain(this.state.selectedSubstitutionValues).values().flatMap(_.values).every().value();
  };
  submit = () => {
    this.props.onSubmit(this.state.selectedInferenceSuggestion.inference.id, this.state.selectedSubstitutionValues);
  };

  render() {
    function renderSuggestionsContainer ({containerProps, children}) {
      return <div {...containerProps}><DropdownContainer>{children}</DropdownContainer></div>
    }
    let showSubstitutions = (key, boundVariableLists, addParameter) => {
      const requiredSubstitutions = this.state.selectedInferenceSuggestion.requiredSubstitutions[key];
      if (addParameter) {
        boundVariableLists = [...boundVariableLists, ["_"]];
      }

      return requiredSubstitutions.length > 0 && requiredSubstitutions.map(name => {
        const validValues = this.getValidSubstitutionValues(key, name);
        const selectionElement = !validValues ?
          <Form.Control type="text" value={this.state.selectedSubstitutionValues[key][name]} onChange={e => this.setSelectedSubstitutionValue(key, name, e.target.value)}/> :
          validValues.length === 1 ?
          <Form.Label column><Expression expression={validValues[0]} boundVariableLists={boundVariableLists} /></Form.Label> :
          <Form.Control as="select" value={this.state.selectedSubstitutionValues[key][name]} onChange={e => this.setSelectedSubstitutionValue(key, name, e.target.value)}>
            <option value="" />
            {validValues.map(v =>
              <option value={v.serialize()} dangerouslySetInnerHTML={{__html: renderToString(
                <Expression expression={v} boundVariableLists={boundVariableLists} />
              )}}/>
            )}
          </Form.Control>;

        return <Form.Group key={`${key} ${name}`} as={Form.Row}>
          <Form.Label column xs={1}>{name}{addParameter && "(_)"}</Form.Label>
          <Form.Label column xs={1}>&rarr;</Form.Label>
          <Col>{selectionElement}</Col>
        </Form.Group>
      });
    };
    const {show, onHide, boundVariableLists} = this.props;

    return <Modal show={show} onHide={onHide}>
      <Modal.Header closeButton><Modal.Title>Find inference</Modal.Title></Modal.Header>
      <Modal.Body>
        <Form>
          <Form.Group>
            <Form.Label><strong>Select inference</strong></Form.Label>
            <Autosuggest
              suggestions={this.state.inferenceSuggestions}
              onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
              onSuggestionsClearRequested={this.onSuggestionsClearRequested}
              getSuggestionValue={s => s.inference.name}
              renderSuggestionsContainer={renderSuggestionsContainer}
              onSuggestionSelected={this.onSuggestionSelected}
              renderSuggestion={s => <span className="dropdown-item">{s.inference.name}</span>}
              inputProps={{value: this.state.autosuggestValue, onChange: this.onAutosuggestChange, className:"form-control"}} />
          </Form.Group>
          {this.state.selectedInferenceSuggestion &&
            <>
              <Form.Group>
                <InferenceSummary inference={this.state.selectedInferenceSuggestion.inference}/>
              </Form.Group>
              { this.state.premiseSuggestions &&
                  <Form.Group>
                    <Form.Label><strong>Premises</strong></Form.Label>
                    {_.zip(this.state.selectedInferenceSuggestion.inference.premises, this.state.premiseSuggestions).map(([premise, suggestions], i) =>
                      <Form.Group as={Form.Row}>
                        <Col xs={2}>
                          <Expression expression={premise} boundVariableLists={[]} />
                        </Col>
                        <Col>
                          <Form.Control as="select" value={this.state.selectedPremiseSuggestions[i][0]} onChange={(e) => this.setSelectedPremiseSuggestion(i, e.target.value)}>
                            <option value="" />
                            {suggestions.map((s, i) =>
                              <option value={i} dangerouslySetInnerHTML={{__html: renderToString(
                                  <Expression expression={s.statement} boundVariableLists={boundVariableLists} />
                                )}}/>
                            )}
                          </Form.Control>
                        </Col>
                      </Form.Group>
                    )}
                  </Form.Group>
              }
              <Form.Group>
                <Form.Label><strong>Substitutions</strong></Form.Label>
                {_.flatten([
                  showSubstitutions("statements", boundVariableLists),
                  showSubstitutions("terms", boundVariableLists),
                  showSubstitutions("predicates", boundVariableLists, true),
                  showSubstitutions("functions", boundVariableLists, true),
                ])}
              </Form.Group>
            </>}
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={this.props.onHide}>Close</Button>
        <Button variant="primary" onClick={this.submit} disabled={!this.readyToSubmit()}>Save Changes</Button>
      </Modal.Footer>
    </Modal>
  }
}

export class BoundVariableModal extends React.Component {
  render() {
    const {show, onHide, title, value, onChange, onSave} = this.props;
    return <Modal show={show} onHide={onHide}>
      <Modal.Header closeButton><Modal.Title>{title}</Modal.Title></Modal.Header>
      <Modal.Body>
        <Form>
          <Form.Group>
            <Form.Label>Bound variable name</Form.Label>
            <Form.Control type="text" value={value} onChange={onChange}/>
          </Form.Group>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>Close</Button>
        <Button variant="primary" onClick={onSave}>Save Changes</Button>
      </Modal.Footer>
    </Modal>
  }
}
