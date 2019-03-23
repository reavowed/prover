import React from "react";
import Modal from "react-bootstrap/Modal";
import Form from "react-bootstrap/Form";
import Button from "react-bootstrap/Button";
import Autosuggest from "react-autosuggest";
import styled from "styled-components";
import {InferenceSummary} from "./InferenceSummary";
import {Parser} from "../Parser";
import Col from "react-bootstrap/Col";
import {Expression} from "./Expression";
import {renderToString} from "react-dom/server";

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

export class FindInferenceModal extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      isLoading: false,
      autosuggestValue: "",
      inferenceSuggestions: [],
      selectedInferenceSuggestion: null
    };
  }
  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.findInferences(value)
      .then(response => response.json())
      .then(suggestionsJson => this.setState({inferenceSuggestions: Parser.parseInferenceSuggestions(suggestionsJson)}))
  };
  onSuggestionsClearRequested = () => {
    this.setState({
      inferenceSuggestions: []
    });
  };
  onSuggestionSelected = (event, {suggestion}) => {
    const selectedSubstitutions = _.fromPairs(AllSubstitutionTypes.map(type => {
      const requiredNames = suggestion.requiredSubstitutions[type];
      const namesToSelectedValues = _.fromPairs(_.map(requiredNames, name => {
        const possibleValues = suggestion.substitutions.map(s => s[type][name]);
        return [name, possibleValues.length === 1 ? possibleValues[0].serialize() : ""];
      }));
      return [type, namesToSelectedValues];
    }));
    this.setState({
      selectedInferenceSuggestion: suggestion,
      selectedSubstitutions
    });
  };
  setSubstitutionValue = (key, name, value) => {
    const {selectedSubstitutions} = this.state;
    selectedSubstitutions[key][name] = value;
    this.updateForcedSubstitutionValues(selectedSubstitutions);
    this.setState({selectedSubstitutions});
  };
  readyToSubmit() {
    return this.state.selectedInferenceSuggestion && _.chain(this.state.selectedSubstitutions).values().flatMap(_.values).every().value();
  };
  submit = () => {
    this.props.onSubmit(this.state.selectedInferenceSuggestion.inference.id, this.state.selectedSubstitutions);
  };
  getCompatibleSubstitutions = (selectedSubstitutions, ignoringType, ignoringName) => {
    return _.filter(this.state.selectedInferenceSuggestion.substitutions, s =>
      _.every(AllSubstitutionTypes, type =>
        _.every(this.state.selectedInferenceSuggestion.requiredSubstitutions[type], name =>
          (ignoringType === type && ignoringName === name) ||
            selectedSubstitutions[type][name] === '' ||
            !s[type][name] ||
            selectedSubstitutions[type][name] === s[type][name].serialize()
        )
      )
    );
  };
  updateForcedSubstitutionValues = (selectedSubstitutions) => {
    const compatibleSubstitutions = this.getCompatibleSubstitutions(selectedSubstitutions);
    _.each(AllSubstitutionTypes, type => {
      _.each(this.state.selectedInferenceSuggestion.requiredSubstitutions[type], name => {
        if (selectedSubstitutions[type][name] === '') {
          const compatibleValues = _.uniq(_.map(compatibleSubstitutions, s => s[type][name]));
          if (compatibleValues.length === 1 && compatibleValues[0]) {
            selectedSubstitutions[type][name] = compatibleValues[0].serialize();
          }
        }
      });
    });
  };
  getValidSubstitutionValues = (type, name) => {
    return this.getCompatibleSubstitutions(this.state.selectedSubstitutions, type, name).map(s => s[type][name]);
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
        const selectionElement = _.some(validValues, _.isUndefined) ?
          <Form.Control type="text" value={this.state.selectedSubstitutions[key][name]} onChange={e => this.setSubstitutionValue(key, name, e.target.value)}/> :
          validValues.length === 1 ?
          <Form.Label column><Expression expression={validValues[0]} boundVariableLists={boundVariableLists} /></Form.Label> :
          <Form.Control as="select" value={this.state.selectedSubstitutions[key][name]} onChange={e => this.setSubstitutionValue(key, name, e.target.value)}>
            <option value="" />
            {validValues.map(v =>
              <option value={v.serialize()} dangerouslySetInnerHTML={{__html: renderToString(
                <Expression expression={v} boundVariableLists={boundVariableLists} />
              )}}/>
            )}
          </Form.Control>

        return <Form.Group key={`${key} ${name}`}>
          <Form.Row >
            <Col xs={1}>
              <Form.Label column>{name}{addParameter && "(_)"}</Form.Label>
            </Col>
            <Col xs={1}>
              <Form.Label column>&rarr;</Form.Label>
            </Col>
            <Col>
              {selectionElement}
            </Col>
          </Form.Row>
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
              <Form.Label><strong>Substitutions</strong></Form.Label>
              {_.flatten([
                showSubstitutions("statements", boundVariableLists),
                showSubstitutions("terms", boundVariableLists),
                showSubstitutions("predicates", boundVariableLists, true),
                showSubstitutions("functions", boundVariableLists, true),
              ])}
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
