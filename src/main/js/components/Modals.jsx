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
import path from 'path';

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
    const substitutions = _.fromPairs(["statements", "terms", "predicates", "functions"].map(key => {
      const required = suggestion.requiredSubstitutions[key];
      const values = _.fromPairs(_.map(required, name => {
        const given = suggestion.substitutions[0][key][name];
        return [name, given ? given.serialize() : ""];
      }));
      return [key, values];
    }));
    this.setState({
      selectedInferenceSuggestion: suggestion,
      substitutions: substitutions
    });
  };
  setSubstitution = (key, name, value) => {
    const substitutions = this.state.substitutions;
    substitutions[key][name] = value;
    this.setState({
      substitutions: substitutions
    });
  };
  readyToSubmit() {
    return this.state.selectedInferenceSuggestion && _.chain(this.state.substitutions).values().flatMap(_.values).every().value();
  };
  submit = () => {
    this.props.onSubmit(this.state.selectedInferenceSuggestion.inference.id, this.state.substitutions);
  };

  render() {
    function renderSuggestionsContainer ({containerProps, children}) {
      return <div {...containerProps}><DropdownContainer>{children}</DropdownContainer></div>
    }
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
                {["statements", "terms", "predicates", "functions"].map(key => {
                  const requiredSubstitutions = this.state.selectedInferenceSuggestion.requiredSubstitutions[key];
                  const givenSubstitutions = this.state.selectedInferenceSuggestion.substitutions[0][key];
                  return requiredSubstitutions.length > 0 && requiredSubstitutions.map(name =>
                    <Form.Group key={`${key} ${name}`}>
                      <Form.Row >
                        <Col xs={1}>
                          <Form.Label column>{name}</Form.Label>
                        </Col>
                        <Col xs={1}>
                          <Form.Label column>&rarr;</Form.Label>
                        </Col>
                        <Col>
                          {givenSubstitutions[name] ?
                            <Form.Label column><Expression expression={givenSubstitutions[name]} boundVariableLists={boundVariableLists} /></Form.Label> :
                            <Form.Control type="text" value={this.state.substitutions[key][name]} onChange={e => this.setSubstitution(key, name, e.target.value)}/>}
                        </Col>
                      </Form.Row>
                    </Form.Group>);
                })}
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
