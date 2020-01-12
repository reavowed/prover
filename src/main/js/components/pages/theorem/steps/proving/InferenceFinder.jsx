import _ from "lodash";
import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {Parser} from "../../../../../Parser";
import {CopiableExpression, ExpressionComponent} from "../../../../ExpressionComponent";
import InputWithShorthandReplacement from "../../../../helpers/InputWithShorthandReplacement";
import InferenceAutosuggest from "./InferenceAutosuggest";
import {InferenceSummary} from "../../../../InferenceSummary";
import SuggestionDropdownElement from "./SuggestionDropdownElement";
import BoundVariableLists from "../BoundVariableLists";
import ProofContext from "../../ProofContext";

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
  static contextType = ProofContext;
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
          this.setState({inferenceSuggestions: this.context.parser.parseInferenceSuggestions(suggestionsJson)})
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
      const substitutions = this.context.parser.parseSubstitutions(suggestionsJson.immediateSubstitutions);
      const selectedSubstitutionValues = this.getAllForcedSubstitutionValues([[substitutions]], this.state.selectedInferenceSuggestion.requiredSubstitutions);
      this.setState({selectedSubstitutionValues});
      this.submitWithSelectedValues(this.state.selectedInferenceSuggestion, selectedSubstitutionValues);
    } else {
      this.setState({
        premiseSuggestions: this.context.parser.parsePremiseSuggestions(suggestionsJson.premiseMatches),
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
      .then(this.context.parser.parseSubstitutions)
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
    let PremiseSuggestions = () => {
      const boundVariableLists = useContext(BoundVariableLists) || [];
      return <Form.Group>
        <Form.Label><strong>Premises</strong></Form.Label>
        {_.zip(this.state.selectedInferenceSuggestion.inference.premises, this.state.premiseSuggestions).map(([premise, suggestions], i) =>
          <Form.Group as={Form.Row} key={i}>
            <Col xs={4}>
              <CopiableExpression expression={premise} />
            </Col>
            <Col>
              <Form.Control as="select" value={this.state.selectedPremiseSuggestions[i][0]} onChange={(e) => this.setSelectedPremiseSuggestion(i, e.target.value)}>
                <option value="" />
                {suggestions.map((s, i) =>
                  <option key={i} value={i} dangerouslySetInnerHTML={{__html: renderToString(
                      <ExpressionComponent expression={s.statement} boundVariableLists={boundVariableLists} />
                    )}}/>
                )}
              </Form.Control>
            </Col>
          </Form.Group>
        )}
      </Form.Group>
    };
    let showSubstitutionOptions = (name, validValues, getter, setter) => {
      const selectionElement = !validValues ?
        <InputWithShorthandReplacement value={getter(this.state.selectedSubstitutionValues)}
                                       onChange={e => this.setSelectedSubstitutionValue(setter, ...this.context.parser.replaceShorthands(e))}
                                       onKeyUp={this.onInputKeyUp} /> :
        validValues.length === 1 ?
          <Form.Label column><CopiableExpression expression={validValues[0]} /></Form.Label> :
          <BoundVariableLists.Consumer>{ boundVariableLists =>
            <Form.Control as="select" value={getter(this.state.selectedSubstitutionValues)} onChange={e => this.setSelectedSubstitutionValue(setter, e.target.value)}>
              <option value="" />
              {validValues.map(v =>
                <option key={v.serialize()} value={v.serialize()} dangerouslySetInnerHTML={{__html: renderToString(
                    <ExpressionComponent expression={v} boundVariableLists={boundVariableLists} />
                  )}}/>
              )}
            </Form.Control>
          }</BoundVariableLists.Consumer>;

      return <Form.Group as={Form.Row}>
        <Form.Label column xs={2}><CopiableExpression expression={{textForHtml: () => name}}/></Form.Label>
        <Form.Label column xs={1}>&rarr;</Form.Label>
        <Col>{selectionElement}</Col>
      </Form.Group>
    };
    let showSimpleSubstitutions = (key) => {
      const requiredSubstitutions = this.state.selectedInferenceSuggestion.requiredSubstitutions[key];
      return requiredSubstitutions.length > 0 && requiredSubstitutions.map(name => {
        const validValues = this.getValidSubstitutionValues(key, name);
        return <React.Fragment key={`${key} ${name}`}>
          {showSubstitutionOptions(name, validValues, x => x[key][name], (x, y) => x[key][name] = y)}
        </React.Fragment>;
      });
    };
    let showParameteredSubstitutions = (key) => {
      const requiredSubstitutions = this.state.selectedInferenceSuggestion.requiredSubstitutions[key];
      return requiredSubstitutions.length > 0 && requiredSubstitutions.map(([name, numberOfParameters]) => {
        const validValues = this.getValidSubstitutionValues(key, name, numberOfParameters);
        const newVariableList = numberOfParameters === 1 ? ["$"] : _.map(_.range(numberOfParameters), x => "$_" + (x+1));
        return <BoundVariableLists.AddParameters variables={newVariableList} key={`${key} ${name} ${numberOfParameters}`}>
          {showSubstitutionOptions(`${name}(${newVariableList.join(", ")})`, validValues, x => x[key][name][numberOfParameters], (x, y) => x[key][name][numberOfParameters] = y)}
        </BoundVariableLists.AddParameters>
      });
    };
    const {title, autofocus} = this.props;

    let getSuggestionValue = s => s.rewriteInference ? s.inference.name + " [" + s.rewriteInference.name + "]" : s.inference.name;
    let renderSuggestion = s => <SuggestionDropdownElement
      mainElement={getSuggestionValue(s)}
      hoverElement={<CopiableExpression expression={s.conclusion} />} />;

    return <>
      <Form.Group>
        <Form.Label><strong>{title}</strong></Form.Label>
        <InferenceAutosuggest
          autofocus={autofocus}
          value={this.state.autosuggestValue}
          onValueChange={this.onAutosuggestChange}
          suggestions={this.state.inferenceSuggestions}
          getSuggestionValue={getSuggestionValue}
          renderSuggestion={renderSuggestion}
          onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
          onSuggestionsClearRequested={this.onSuggestionsClearRequested}
          onSuggestionSelected={this.onSuggestionSelected} />
      </Form.Group>
      {this.state.selectedInferenceSuggestion && <>
        <Form.Group>
          <InferenceSummary inference={this.state.selectedInferenceSuggestion.inference}/>
        </Form.Group>
        { this.state.premiseSuggestions && <PremiseSuggestions/> }
        <Form.Group>
          <Form.Label><strong>Substitutions</strong></Form.Label>
          {_.flatten([
            showSimpleSubstitutions("statements"),
            showSimpleSubstitutions("terms"),
            showParameteredSubstitutions("predicates"),
            showParameteredSubstitutions("functions"),
          ])}
        </Form.Group>
        <div className="text-center">
          <Button variant="primary" onClick={this.submit} disabled={!this.readyToSubmit()}>Save Changes</Button>
        </div>
      </>}
    </>
  }
}
