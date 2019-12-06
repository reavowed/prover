import _ from "lodash";
import React from "react";
import Autosuggest from "react-autosuggest";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {Parser} from "../Parser";
import {CopiableExpression} from "./ExpressionComponent";
import {InferenceSummary} from "./InferenceSummary";
import styled from "styled-components";

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

function simpleGetter(type, name) {
  return substitutions => substitutions[type][name];
}
function applicationGetter(type, applicationType, name, length) {
  return substitutions => {
    const baseValue = substitutions[type][name] && substitutions[type][name][length];
    if (!_.isUndefined(baseValue)) {
      return baseValue;
    } else {
      return substitutions[applicationType] && substitutions[applicationType][name] && substitutions[applicationType][name][length];
    }
  };
}

function buildSubstitutionMap(requiredSubstitutions, f) {
  const map = {};
  function getSimple(type) {
    map[type] = _.fromPairs(requiredSubstitutions[type].map(name => [name, f(simpleGetter(type, name))]));
  }
  function getParametered(type, applicationType) {
    map[type] = Parser.doubleMapFromTriples(requiredSubstitutions[type].map(([name, length]) => [name, length, f(applicationGetter(type, applicationType, name, length))]));
  }
  getSimple("statements");
  getSimple("terms");
  getParametered("predicates", "predicateApplications");
  getParametered("functions", "functionApplications");
  return map;
}

function getAllRequiredPaths(requiredSubstitutions) {
  function getSimple(type) {
    return _.map(requiredSubstitutions[type], name => simpleGetter(type, name));
  }
  function getParametered(type, applicationType) {
    return _.map(requiredSubstitutions[type], ([name, length]) => applicationGetter(type, applicationType, name, length));
  }
  return [
    ...getSimple("statements"),
    ...getSimple("terms"),
    ...getParametered("predicates", "predicateApplications"),
    ...getParametered("functions", "functionApplications")
  ];
}

function getAtPath(substitutions, path) {
  return path(substitutions);
}

export class InferenceFinder extends React.Component {
  constructor(...args) {
    super(...args);
    this.autoSuggestRef = React.createRef();
    this.state = {
      isLoading: false,
      autosuggestValue: "",
      inferenceSuggestions: [],
      premiseSuggestions: null,
      selectedInferenceSuggestion: null,
      selectedPremiseSuggestions: []
    };
  }
  componentDidMount() {
    if (this.props.focusOnMount) {
      this.autoSuggestRef.current.input.focus();
    }
  }
  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.getInferenceSuggestions(value)
      .then(suggestionsJson => {
        if (this.state.autosuggestValue === value) {
          this.setState({inferenceSuggestions: Parser.parseInferenceSuggestions(suggestionsJson)})
        }
      })
  };
  onSuggestionsClearRequested = () => {
    this.setState({
      inferenceSuggestions: []
    });
  };
  onSuggestionSelected = (event, {suggestion}) => {
    const forcedSubstitutionValues = this.getAllForcedSubstitutionValues(_.filter([suggestion.substitutions]), suggestion.requiredSubstitutions);
    const selectedSubstitutionValues = buildSubstitutionMap(suggestion.requiredSubstitutions, getter => getter(forcedSubstitutionValues) ? getter(forcedSubstitutionValues) : "");
    this.setState({
      selectedInferenceSuggestion: suggestion,
      premiseSuggestions: null,
      selectedSubstitutionValues
    });
    if (this.areSubstitutionValuesSufficient(suggestion, selectedSubstitutionValues)) {
      this.submitWithSelectedValues(suggestion, selectedSubstitutionValues);
    } else {
      this.props.getPremiseSuggestions &&
      this.props.getPremiseSuggestions(suggestion.inference.id)
        .then(this.handlePremiseSuggestions)
    }
  };
  handlePremiseSuggestions = (suggestionsJson) => {
    if (suggestionsJson.immediateSubstitutions) {
      const substitutions = Parser.parseSubstitutions(suggestionsJson.immediateSubstitutions);
      const selectedSubstitutionValues = this.getAllForcedSubstitutionValues([[substitutions]], this.state.selectedInferenceSuggestion.requiredSubstitutions);
      this.setState({selectedSubstitutionValues});
      this.submitWithSelectedValues(this.state.selectedInferenceSuggestion, selectedSubstitutionValues);
    } else {
      this.setState({
        premiseSuggestions: Parser.parsePremiseSuggestions(suggestionsJson.premiseMatches),
        selectedPremiseSuggestions: this.state.selectedInferenceSuggestion.inference.premises.map(() => ["", null])
      })
    }
  };
  setSelectedPremiseSuggestion = (premiseIndex, suggestionIndex) => {
    const selectedSuggestion = [suggestionIndex, suggestionIndex !== "" ? this.state.premiseSuggestions[premiseIndex][parseInt(suggestionIndex)].substitutions : null];
    const selectedPremiseSuggestions = Object.assign({}, this.state.selectedPremiseSuggestions, {[premiseIndex]: selectedSuggestion});
    const selectedSubstitutionValues = this.updateForcedSubstitutionValues(this.state.selectedSubstitutionValues, selectedPremiseSuggestions);
    this.setState({selectedSubstitutionValues, selectedPremiseSuggestions})
    this.getSubstitutionSuggestions(selectedPremiseSuggestions);
  };

  getSubstitutionSuggestions = (selectedPremiseSuggestions) => {
    if (this.props.getSubstitutionSuggestions && _.keys(_.pickBy(selectedPremiseSuggestions, x => x[1])).length > 1) {
      this.props.getSubstitutionSuggestions(
        this.state.selectedInferenceSuggestion.inference.id,
        _.chain(selectedPremiseSuggestions)
          .pickBy(x => x[1])
          .map((value, key) => [key, this.state.premiseSuggestions[key][parseInt(value[0])].statement])
          .fromPairs()
          .value())
      .then(Parser.parseSubstitutions)
      .then(additionalSubstitutionSuggestions => {
        if (_.isEqual(this.state.selectedPremiseSuggestions, selectedPremiseSuggestions)) {
          const selectedSubstitutionValues = this.updateForcedSubstitutionValues(this.state.selectedSubstitutionValues, selectedPremiseSuggestions, additionalSubstitutionSuggestions);
          this.setState({additionalSubstitutionSuggestions, selectedSubstitutionValues});
        }
      })
    }
  };

  updateForcedSubstitutionValues = (selectedSubstitutionValues, selectedPremiseSuggestions, additionalSubstitutionSuggestions) => {
    const compatibleSubstitutions = this.getPossibleSubstitutionsLists(selectedPremiseSuggestions, selectedSubstitutionValues, additionalSubstitutionSuggestions);
    const forcedSubstitutionValues = this.getAllForcedSubstitutionValues(compatibleSubstitutions, this.state.selectedInferenceSuggestion.requiredSubstitutions);
    const areAnyNewValuesForced = _.some(getAllRequiredPaths( this.state.selectedInferenceSuggestion.requiredSubstitutions), path => {
      const forcedValue = getAtPath(forcedSubstitutionValues, path);
      return forcedValue && forcedValue !== getAtPath(selectedSubstitutionValues, path)
    });
    if (areAnyNewValuesForced) {
      const newSelectedSubstitutionValues = _.merge({}, selectedSubstitutionValues, forcedSubstitutionValues);
      return this.updateForcedSubstitutionValues(newSelectedSubstitutionValues, selectedPremiseSuggestions, additionalSubstitutionSuggestions);
    } else {
      return selectedSubstitutionValues;
    }
  };
  getAllForcedSubstitutionValues = (possibleSubstitutionsLists, requiredSubstitutions) => {
    const selectableSubstitutionValues = this.getAllSelectableSubstitutionValues(possibleSubstitutionsLists, requiredSubstitutions);
    return buildSubstitutionMap(requiredSubstitutions, getter =>
      getter(selectableSubstitutionValues) && getter(selectableSubstitutionValues).length === 1 && getter(selectableSubstitutionValues)[0].serialize() || undefined
    );
  };
  getAllSelectableSubstitutionValues = (possibleSubstitutionsLists, requiredSubstitutions) => {
    return buildSubstitutionMap(requiredSubstitutions, getter => {
      const allowedValuesLists = _.map(possibleSubstitutionsLists, substitutionsList =>
        _.uniqBy(_.flatten(substitutionsList.map(getter)), x => !x || x.serialize())
      );
      const allowAnything = _.every(allowedValuesLists, allowedValues => _.some(allowedValues, _.isUndefined));
      if (allowAnything) {
        return null;
      }
      const listsWithRestrictions = _.filter(allowedValuesLists, allowedValues => !_.some(allowedValues, _.isUndefined));
      return _.intersectionBy(...listsWithRestrictions, x => x.serialize());
    });
  };

  getPossibleSubstitutionsLists = (selectedPremiseSuggestions, selectedSubstitutionValues, additionalSubstitutionSuggestions) => {
    const possibleSubstitutionsFromInferenceAndPremises = _.filter([(additionalSubstitutionSuggestions ? [additionalSubstitutionSuggestions] : this.state.selectedInferenceSuggestion.substitutions), ..._.map(selectedPremiseSuggestions, s => s[1])]);
    return _.map(possibleSubstitutionsFromInferenceAndPremises, possibleSubstitutions =>
      _.filter(possibleSubstitutions, s =>
        _.every(getAllRequiredPaths(this.state.selectedInferenceSuggestion.requiredSubstitutions), path => {
          const selectedValue = getAtPath(selectedSubstitutionValues, path);
          const value = getAtPath(s, path);
          return selectedValue === '' || !value || (_.isArray(value) ? _.some(value, v => selectedValue === v.serialize()) : selectedValue === value.serialize());
        })
      )
    );
  };

  getValidSubstitutionValues = (type, name, numberOfParameters) => {
    const selectedSubstitutionValues = _.cloneDeep(this.state.selectedSubstitutionValues);
    numberOfParameters ? selectedSubstitutionValues[type][name][numberOfParameters] = "" : selectedSubstitutionValues[type][name] = "";
    const compatibleSubstitutions = this.getPossibleSubstitutionsLists(this.state.selectedPremiseSuggestions, selectedSubstitutionValues, this.state.additionalSubstitutionSuggestions);
    const allSelectableValues = this.getAllSelectableSubstitutionValues(compatibleSubstitutions, this.state.selectedInferenceSuggestion.requiredSubstitutions)
    return numberOfParameters ? allSelectableValues[type][name][numberOfParameters] : allSelectableValues[type][name];
  };

  setSelectedSubstitutionValue = (setter, value, callback) => {
    let selectedSubstitutionValues = _.cloneDeep(this.state.selectedSubstitutionValues);
    setter(selectedSubstitutionValues, value);
    selectedSubstitutionValues = this.updateForcedSubstitutionValues(selectedSubstitutionValues, this.state.selectedPremiseSuggestions, this.state.additionalSubstitutionSuggestions);
    this.setState({selectedSubstitutionValues}, callback);
  };
  onInputKeyUp = (event) => {
    if (event.keyCode === 13 && this.readyToSubmit()) {
      this.submit();
    }
    event.preventDefault();
    event.stopPropagation();
  };

  areSubstitutionValuesSufficient(selectedInferenceSuggestion, selectedSubstitutionValues) {
    return selectedInferenceSuggestion && _.every(
      getAllRequiredPaths(selectedInferenceSuggestion.requiredSubstitutions),
      path => getAtPath(selectedSubstitutionValues, path));
  }
  readyToSubmit() {
    return this.areSubstitutionValuesSufficient(this.state.selectedInferenceSuggestion, this.state.selectedSubstitutionValues);
  };
  submit = () => {
    this.submitWithSelectedValues(this.state.selectedInferenceSuggestion, this.state.selectedSubstitutionValues);
  };
  submitWithSelectedValues = (selectedInferenceSuggestion, selectedSubstitutionValues) => {
    this.props.submit(selectedInferenceSuggestion, selectedSubstitutionValues || this.state.selectedSubstitutionValues);
  };

  render() {
    function renderSuggestionsContainer ({containerProps, children}) {
      return <div {...containerProps}><DropdownContainer>{children}</DropdownContainer></div>
    }
    let showSubstitutionOptions = (name, key, validValues, boundVariableLists, getter, setter) => {
      const selectionElement = !validValues ?
        <Form.Control type="text"
                      value={getter(this.state.selectedSubstitutionValues)}
                      onChange={e => this.setSelectedSubstitutionValue(setter, ...Parser.replaceShorthands(e))}
                      onKeyUp={this.onInputKeyUp}
        /> :
        validValues.length === 1 ?
          <Form.Label column><CopiableExpression expression={validValues[0]} boundVariableLists={boundVariableLists} /></Form.Label> :
          <Form.Control as="select" value={getter(this.state.selectedSubstitutionValues)} onChange={e => this.setSelectedSubstitutionValue(setter, e.target.value)}>
            <option value="" />
            {validValues.map(v =>
              <option key={v.serialize()} value={v.serialize()} dangerouslySetInnerHTML={{__html: renderToString(
                  <CopiableExpression expression={v} boundVariableLists={boundVariableLists} />
                )}}/>
            )}
          </Form.Control>;

      return <Form.Group key={key} as={Form.Row}>
        <Form.Label column xs={2}><CopiableExpression expression={{textForHtml: () => name}} boundVariableLists={[]}/></Form.Label>
        <Form.Label column xs={1}>&rarr;</Form.Label>
        <Col>{selectionElement}</Col>
      </Form.Group>
    };
    let showSimpleSubstitutions = (key, boundVariableLists) => {
      const requiredSubstitutions = this.state.selectedInferenceSuggestion.requiredSubstitutions[key];
      return requiredSubstitutions.length > 0 && requiredSubstitutions.map(name => {
        const validValues = this.getValidSubstitutionValues(key, name);
        return showSubstitutionOptions(name, `${key} ${name}`, validValues, boundVariableLists, x => x[key][name], (x, y) => x[key][name] = y);
      });
    };
    let showParameteredSubstitutions = (key, boundVariableLists) => {
      const requiredSubstitutions = this.state.selectedInferenceSuggestion.requiredSubstitutions[key];
      return requiredSubstitutions.length > 0 && requiredSubstitutions.map(([name, numberOfParameters]) => {
        const validValues = this.getValidSubstitutionValues(key, name, numberOfParameters);
        const newVariableList = numberOfParameters === 1 ? ["$"] : _.map(_.range(numberOfParameters), x => "$_" + (x+1));
        return showSubstitutionOptions(`${name}(${newVariableList.join(", ")})`, `${key} ${name} ${numberOfParameters}`, validValues, [...boundVariableLists, newVariableList], x => x[key][name][numberOfParameters], (x, y) => x[key][name][numberOfParameters] = y);
      });
    };
    const {title, boundVariableLists} = this.props;

    let getSuggestionText = s => s.rewriteInference ? s.inference.name + " [" + s.rewriteInference.name + "]" : s.inference.name;

    return <>
      <Form.Group>
        <Form.Label><strong>{title}</strong></Form.Label>
        <Autosuggest
          ref={this.autoSuggestRef}
          suggestions={this.state.inferenceSuggestions}
          onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
          onSuggestionsClearRequested={this.onSuggestionsClearRequested}
          shouldRenderSuggestions={() => true}
          getSuggestionValue={getSuggestionText}
          renderSuggestionsContainer={renderSuggestionsContainer}
          onSuggestionSelected={this.onSuggestionSelected}
          renderSuggestion={s => <span className="dropdown-item">{getSuggestionText(s)}</span>}
          inputProps={{value: this.state.autosuggestValue, onChange: this.onAutosuggestChange, className:"form-control"}} />
      </Form.Group>
      {this.state.selectedInferenceSuggestion && <>
        <Form.Group>
          <InferenceSummary inference={this.state.selectedInferenceSuggestion.inference}/>
        </Form.Group>
        { this.state.premiseSuggestions &&
        <Form.Group>
          <Form.Label><strong>Premises</strong></Form.Label>
          {_.zip(this.state.selectedInferenceSuggestion.inference.premises, this.state.premiseSuggestions).map(([premise, suggestions], i) =>
            <Form.Group as={Form.Row} key={i}>
              <Col xs={4}>
                <CopiableExpression expression={premise} boundVariableLists={[]} />
              </Col>
              <Col>
                <Form.Control as="select" value={this.state.selectedPremiseSuggestions[i][0]} onChange={(e) => this.setSelectedPremiseSuggestion(i, e.target.value)}>
                  <option value="" />
                  {suggestions.map((s, i) =>
                    <option key={i} value={i} dangerouslySetInnerHTML={{__html: renderToString(
                        <CopiableExpression expression={s.statement} boundVariableLists={boundVariableLists} />
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
            showSimpleSubstitutions("statements", boundVariableLists),
            showSimpleSubstitutions("terms", boundVariableLists),
            showParameteredSubstitutions("predicates", boundVariableLists),
            showParameteredSubstitutions("functions", boundVariableLists),
          ])}
        </Form.Group>
        <div className="text-center">
          <Button variant="primary" onClick={this.submit} disabled={!this.readyToSubmit()}>Save Changes</Button>
        </div>
      </>}
    </>
  }
}
