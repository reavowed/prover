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
import {InferenceSummary} from "../../../../../InferenceSummary";
import BoundVariableLists from "../../BoundVariableLists";

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

export default class ConclusionChooser extends React.Component {
  constructor(props) {
    super(props);
    this.ref = React.createRef();
    this.state = {
      selectedConclusion: null
    };
  }

  componentDidMount() {
    this.ref.current.scrollIntoView();
    const conclusionToSelect = this.props.possibleConclusions.length === 1 ?
      this.props.possibleConclusions[0] :
      _.find(this.props.possibleConclusions, c => c.conclusion.serialize() === this.props.defaultConclusionStatement.serialize());
    if (conclusionToSelect) {
      this.setSelectedConclusion(conclusionToSelect);
    }
  }

  setSelectedConclusion = (selectedConclusion) => {
    if (selectedConclusion) {
      const selectedPremises = selectedConclusion.possiblePremises.map(p => ["", null]);
      const selectedSubstitutionValues = buildSubstitutionMap(selectedConclusion.requiredSubstitutions, () => "");
      this.setState({selectedConclusion, selectedPremises, selectedSubstitutionValues});
      if (this.props.allowAutoSubmit && this.areSubstitutionValuesSufficient(selectedConclusion, selectedPremises, selectedSubstitutionValues)) {
        this.submitWithSelectedValues(selectedConclusion, selectedPremises, selectedSubstitutionValues)
      }
    } else {
      this.setState({selectedConclusion: null})
    }
  };

  setSelectedPremise = (premiseIndex, matchIndex) => {
    const selectedPremise = [matchIndex, matchIndex !== "" ? this.state.selectedConclusion.possiblePremises[premiseIndex].possibleMatches[parseInt(matchIndex)].substitutions : null];
    const selectedPremises = Object.assign({}, this.state.selectedPremises, {[premiseIndex]: selectedPremise});
    this.setState({selectedPremises});
  };

  setSelectedSubstitutionValue = (setter, value, callback) => {
    let selectedSubstitutionValues = _.cloneDeep(this.state.selectedSubstitutionValues);
    setter(selectedSubstitutionValues, value);
    this.setState({selectedSubstitutionValues}, callback);
  };

  getValidSubstitutionValues = (getter) => {
    const applicableSubstitutions = this.getApplicableSubstitutions(this.state.selectedConclusion, this.state.selectedPremises);
    const selectableSubstitutionValues = this.getAllSelectableSubstitutionValues(applicableSubstitutions, this.state.selectedConclusion.requiredSubstitutions);
    return getter(selectableSubstitutionValues);
  };

  getApplicableSubstitutions = (selectedConclusion, selectedPremises) => {
    return _.filter([selectedConclusion.substitutions, ..._.map(selectedPremises, s => s[1])])
  };

  getAllSelectableSubstitutionValues = (applicableSubstitutions, requiredSubstitutions) => {
    return buildSubstitutionMap(requiredSubstitutions, getter => {
      const allowedValuesLists = _.map(applicableSubstitutions, substitutions => {
        const value = getter(substitutions);
        return (_.isUndefined(value) || _.isArray(value)) ? value : [value];
      });
      const allowAnything = _.every(allowedValuesLists, _.isUndefined);
      if (allowAnything) {
        return null;
      }
      return _.intersectionBy(..._.filter(allowedValuesLists), x => x.serialize());
    });
  };

  onInputKeyUp = (event) => {
    if (event.keyCode === 13 && this.readyToSubmit()) {
      this.submit();
    }
    event.preventDefault();
    event.stopPropagation();
  };

  getSubstitutionValuesToSubmit = (selectedConclusion, selectedPremises, selectedSubstitutionValues) => {
    const applicableSubstitutions = this.getApplicableSubstitutions(selectedConclusion, selectedPremises);
    const selectableSubstitutionValues = this.getAllSelectableSubstitutionValues(applicableSubstitutions, selectedConclusion.requiredSubstitutions);
    return buildSubstitutionMap(selectedConclusion.requiredSubstitutions, getter => {
      const selectedValue = getter(selectedSubstitutionValues);
      const selectableValues = getter(selectableSubstitutionValues);
      return selectedValue || (selectableValues && selectableValues.length === 1 && selectableValues[0].serialize());
    });
  };

  areSubstitutionValuesSufficient = (selectedConclusion, selectedPremises, selectedSubstitutionValues) => {
    if (!selectedConclusion) return false;
    const valuesToSubmit = this.getSubstitutionValuesToSubmit(selectedConclusion, selectedPremises, selectedSubstitutionValues);
    return _.every(
      getAllRequiredPaths(selectedConclusion.requiredSubstitutions),
      getter => getter(valuesToSubmit));
  };

  readyToSubmit() {
    return this.areSubstitutionValuesSufficient(this.state.selectedConclusion, this.state.selectedPremises, this.state.selectedSubstitutionValues);
  };

  submit = () => {
    const substitutionValues = this.getSubstitutionValuesToSubmit(this.state.selectedConclusion, this.state.selectedPremises, this.state.selectedSubstitutionValues);
    this.props.submit(this.state.selectedConclusion, substitutionValues);
  };

  render() {
    const {possibleConclusions, hideSummary, disabled} = this.props;
    const {selectedConclusion} = this.state;
    return <EntryContext.Consumer>{entryContext => {

      let PremiseSuggestions = () => {
        const boundVariableLists = useContext(BoundVariableLists) || [];
        return <Form.Group>
          <Form.Label><strong>Premises</strong></Form.Label>
          {selectedConclusion.possiblePremises.map(({premise, possibleMatches}, i) =>
            <Form.Group as={Form.Row} key={i}>
              <Col xs={4}>
                <CopiableExpression expression={premise} boundVariableLists={[]}/>
              </Col>
              <Col>
                <Form.Control as="select" value={this.state.selectedPremises[i][0]} onChange={(e) => this.setSelectedPremise(i, e.target.value)} readOnly={disabled}>
                  <option value="" />
                  {possibleMatches.map(({matchingPremise}, i) =>
                    <option key={i} value={i} dangerouslySetInnerHTML={{__html: renderToString(
                        <ExpressionComponent expression={matchingPremise} boundVariableLists={boundVariableLists} entryContext={entryContext} />
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
                                         readOnly={disabled}
                                         onChange={(value, callback) => this.setSelectedSubstitutionValue(setter, value, callback)}
                                         onKeyUp={this.onInputKeyUp} /> :
          validValues.length === 1 ?
            <Form.Label column><CopiableExpression expression={validValues[0]} /></Form.Label> :
            <BoundVariableLists.Consumer>{ boundVariableLists =>
              <Form.Control as="select" value={getter(this.state.selectedSubstitutionValues)} onChange={e => this.setSelectedSubstitutionValue(setter, e.target.value)} readOnly={disabled}>
                <option value="" />
                {validValues.map(v =>
                  <option key={v.serialize()} value={v.serialize()} dangerouslySetInnerHTML={{__html: renderToString(
                      <ExpressionComponent expression={v} boundVariableLists={boundVariableLists} entryContext={entryContext}/>
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
        const requiredSubstitutions = selectedConclusion.requiredSubstitutions[key];
        return requiredSubstitutions.length > 0 && requiredSubstitutions.map(name => {
          const getter = x => x[key][name];
          const setter = (x, y) => x[key][name] = y;
          const validValues = this.getValidSubstitutionValues(getter, setter);
          return <React.Fragment key={`${key} ${name}`}>
            {showSubstitutionOptions(name, validValues, getter, setter)}
          </React.Fragment>;
        });
      };
      let showParameteredSubstitutions = (key) => {
        const requiredSubstitutions = selectedConclusion.requiredSubstitutions[key];
        return requiredSubstitutions.length > 0 && requiredSubstitutions.map(([name, numberOfParameters]) => {
          const getter = x => x[key][name][numberOfParameters];
          const setter = (x, y) => x[key][name][numberOfParameters] = y;
          const validValues = this.getValidSubstitutionValues(getter, setter);
          const newVariableList = numberOfParameters === 1 ? ["$"] : _.map(_.range(numberOfParameters), x => "$_" + (x+1));
          return <BoundVariableLists.AddParameters variables={newVariableList} key={`${key} ${name} ${numberOfParameters}`}>
            {showSubstitutionOptions(`${name}(${newVariableList.join(", ")})`, validValues, getter, setter)}
          </BoundVariableLists.AddParameters>
        });
      };

      return <div ref={this.ref}>
        {possibleConclusions.length > 1 && <Form.Group>
          <Form.Label><strong>Choose conclusion</strong></Form.Label>
          <Form.Control as="select"
                        value={selectedConclusion ? _.indexOf(possibleConclusions, selectedConclusion) : ""}
                        onChange={e => this.setSelectedConclusion(possibleConclusions[e.target.value])} readOnly={disabled}>
            <option value="" />
            {possibleConclusions.map(({conclusion}, index) =>
              <option key={index} value={index} dangerouslySetInnerHTML={{__html: renderToString(
                  <ExpressionComponent expression={conclusion} boundVariableLists={[]} entryContext={entryContext}/>
                )}}/>
            )}
          </Form.Control>
        </Form.Group>}
        {selectedConclusion && <>
          {!hideSummary && <Form.Group>
            <InferenceSummary inference={{premises: selectedConclusion.possiblePremises.map(p => p.premise), conclusion: selectedConclusion.conclusion}}/>
          </Form.Group>}
          {getAllRequiredPaths(selectedConclusion.requiredSubstitutions).length > 0 && <>
            {selectedConclusion.possiblePremises.length !== 0 && <PremiseSuggestions/>}
            <Form.Group>
              <Form.Label><strong>Substitutions</strong></Form.Label>
              {_.flatten([
                showSimpleSubstitutions("statements"),
                showSimpleSubstitutions("terms"),
                showParameteredSubstitutions("predicates"),
                showParameteredSubstitutions("functions"),
              ])}
            </Form.Group>
          </>}
        </>}
        <div className="text-center">
          <Button variant="primary" onClick={this.submit} disabled={!this.readyToSubmit() || disabled}>
            {disabled ? <span className="fas fa-spin fa-spinner"/> : "Save Changes"}
          </Button>
        </div>
      </div>
    }}</EntryContext.Consumer>
  }
}
