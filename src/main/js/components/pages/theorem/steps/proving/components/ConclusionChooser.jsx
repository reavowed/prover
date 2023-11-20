import _ from "lodash";
import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {replaceAtIndex} from "../../../../../../models/Helpers";
import DisplayContext from "../../../../../DisplayContext";
import AvailableEntries from "../../../../../AvailableEntries";
import {CopiableExpression, ExpressionComponent} from "../../../../../ExpressionComponent";
import {InlineTextEditor} from "../../../../../helpers/InlineTextEditor";
import InputWithShorthandReplacement from "../../../../../helpers/InputWithShorthandReplacement";
import {ResultWithPremises} from "../../../../../ResultWithPremises";
import BoundVariableLists from "../../BoundVariableLists";

function substitutionGetter(type, applicationType, name) {
  return substitutions => {
    const baseValue = substitutions[type][name];
    if (!_.isUndefined(baseValue)) {
      return baseValue;
    } else {
      return substitutions[applicationType] && substitutions[applicationType][name];
    }
  };
}

function buildSubstitutionMap(variableDefinitions, f) {
  function callOnValues(type, applicationType) {
    return {[type]: _.range(variableDefinitions[type].length).map(i => f(substitutionGetter(type, applicationType, i)))};
  }
  return {
    ...callOnValues("statements", "statementApplications"),
    ...callOnValues("terms", "termApplications")
  };
}

function getAllRequiredPaths(variableDefinitions) {
  function getPaths(type, applicationType) {
    return _.range(variableDefinitions[type].length).map(i => substitutionGetter(type, applicationType, i));
  }
  return [
    ...getPaths("statements", "statementApplications"),
    ...getPaths("terms", "termApplications")
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
    this.onUpdate();
  }

  componentDidUpdate(prevProps, prevState, snapshot) {
    if (prevProps.possibleConclusions !== this.props.possibleConclusions) {
      this.onUpdate();
    }
  }

  onUpdate() {
    this.ref.current.scrollIntoView();
    const {possibleConclusions, defaultConclusionStatement} = this.props;
    const conclusionIndexToSelect = (possibleConclusions.length > 0 && _.every(possibleConclusions.slice(1), c => _.startsWith(c.extractionDefinition.extractionInferenceIds, possibleConclusions[0].extractionDefinition.extractionInferenceIds))) ?
      0 :
      _.findIndex(possibleConclusions, c => c.conclusion.serialize() === defaultConclusionStatement.serialize());
    const conclusionToSelect = conclusionIndexToSelect === -1 ?
      null :
      possibleConclusions[conclusionIndexToSelect];
    this.setSelectedConclusion(conclusionToSelect, conclusionIndexToSelect);
  }

  setSelectedConclusion = (selectedConclusion, selectedConclusionIndex) => {
    if (selectedConclusion) {
      const fetchConclusionWithPremises = selectedConclusion.possiblePremises ?
        Promise.resolve(selectedConclusion) :
        this.props.fetchPossiblePremises(selectedConclusion);
      return fetchConclusionWithPremises.then(selectedConclusion => {
        const premiseStatements = selectedConclusion.possiblePremises.map(p => p.premise);
        const conclusionStatement = selectedConclusion.conclusion;
        const selectedPremises = selectedConclusion.possiblePremises.map(p => ["", null]);
        const selectedSubstitutionValues = buildSubstitutionMap(selectedConclusion.variableDefinitions, () => "");
        this.setStatePromise({selectedConclusion, selectedConclusionIndex, premiseStatements, conclusionStatement, selectedPremises, selectedSubstitutionValues})
          .then(() => this.ref.current.scrollIntoView());
        if (this.props.allowAutoSubmit && this.areSubstitutionValuesSufficient(selectedConclusion, selectedPremises, selectedSubstitutionValues)) {
          this.submitWithSelectedValues(selectedConclusion, selectedPremises, selectedSubstitutionValues)
        }
      });
    } else {
      this.setStatePromise({selectedConclusion: null, selectedConclusionIndex: null, conclusionStatement: null, selectedPremises: [], selectedSubstitutionValues: {}})
        .then(() => this.ref.current.scrollIntoView());
    }
  };

  setSelectedPremise = (premiseIndex, matchIndex) => {
    const selectedPremise = [matchIndex, matchIndex !== "" ? this.state.selectedConclusion.possiblePremises[premiseIndex].possibleMatches[parseInt(matchIndex)].substitutions : null];
    const selectedPremises = replaceAtIndex(this.state.selectedPremises, premiseIndex, selectedPremise);
    this.setState({selectedPremises});
  };

  setSelectedSubstitutionValue = (setter, value, callback) => {
    let selectedSubstitutionValues = _.cloneDeep(this.state.selectedSubstitutionValues);
    setter(selectedSubstitutionValues, value);
    this.setState({selectedSubstitutionValues}, callback);
  };

  getValidSubstitutionValues = (getter) => {
    const applicableSubstitutions = this.getApplicableSubstitutions(this.state.selectedConclusion, this.state.selectedPremises);
    const selectableSubstitutionValues = this.getAllSelectableSubstitutionValues(applicableSubstitutions, this.state.selectedConclusion.variableDefinitions);
    return getter(selectableSubstitutionValues);
  };

  getApplicableSubstitutions = (selectedConclusion, selectedPremises) => {
    return _.filter([selectedConclusion.substitutions, ..._.map(selectedPremises, s => s[1])])
  };

  getAllSelectableSubstitutionValues = (applicableSubstitutions, variableDefinitions) => {
    return buildSubstitutionMap(variableDefinitions, getter => {
      const allowedValuesLists = _.map(applicableSubstitutions, substitutions => {
        const value = getter(substitutions);
        return (_.isNull(value) || _.isArray(value)) ? value : [value];
      });
      const allowAnything = _.every(allowedValuesLists, _.isNull);
      return allowAnything ? null : _.intersectionBy(..._.filter(allowedValuesLists), x => x.serialize());
    });
  };

  onInputKeyDown = (event) => {
    if (!event.isDefaultPrevented() && event.keyCode === 13 && this.readyToSubmit()) {
      this.submit();
      event.preventDefault();
      event.stopPropagation();
    }
  };

  getSubstitutionValuesToSubmit = (selectedConclusion, selectedPremises, selectedSubstitutionValues) => {
    const applicableSubstitutions = this.getApplicableSubstitutions(selectedConclusion, selectedPremises);
    const selectableSubstitutionValues = this.getAllSelectableSubstitutionValues(applicableSubstitutions, selectedConclusion.variableDefinitions);
    return buildSubstitutionMap(selectedConclusion.variableDefinitions, getter => {
      const selectedValue = getter(selectedSubstitutionValues);
      const selectableValues = getter(selectableSubstitutionValues);
      return selectedValue || (selectableValues && selectableValues.length === 1 && selectableValues[0].serialize());
    });
  };

  areSubstitutionValuesSufficient = (selectedConclusion, selectedPremises, selectedSubstitutionValues) => {
    if (!selectedConclusion) return false;
    const valuesToSubmit = this.getSubstitutionValuesToSubmit(selectedConclusion, selectedPremises, selectedSubstitutionValues);
    return _.every(
      getAllRequiredPaths(selectedConclusion.variableDefinitions),
      getter => getter(valuesToSubmit));
  };

  readyToSubmit() {
    return this.areSubstitutionValuesSufficient(this.state.selectedConclusion, this.state.selectedPremises, this.state.selectedSubstitutionValues);
  };

  submit = () => {
    const substitutionValues = this.getSubstitutionValuesToSubmit(this.state.selectedConclusion, this.state.selectedPremises, this.state.selectedSubstitutionValues);
    this.props.submit(this.state.selectedConclusion, substitutionValues, this.state.premiseStatements, this.state.conclusionStatement);
  };

  render() {
    const {possibleConclusions, hideSummary, disabled, boundVariableListsForPremises, boundVariableListsForSubstitutions, conclusionVariableDefinitions} = this.props;
    const {selectedConclusion, selectedConclusionIndex, premiseStatements, conclusionStatement} = this.state;

    const wrapPremiseBoundVariable = (premiseIndex) => (name, index, boundVariablePath) => {
      const callback = (newName) => {
        const newPremise = premiseStatements[premiseIndex].setBoundVariableName(newName, index, boundVariablePath);
        const newPremiseStatements = replaceAtIndex(premiseStatements, premiseIndex, newPremise);
        return this.setStatePromise({premiseStatements: newPremiseStatements});
      };
      return <InlineTextEditor text={name} callback={callback} />;
    };
    const wrapConclusionBoundVariable = (name, index, boundVariablePath) => {
      const callback = (newName) => {
        const newConclusion = conclusionStatement.setBoundVariableName(newName, index, boundVariablePath);
        return this.setStatePromise({conclusionStatement: newConclusion});
      };
      return <InlineTextEditor text={name} callback={callback} />;
    };

    return <AvailableEntries.Consumer>{availableEntries =>
      <DisplayContext.Consumer>{displayContext => {
        const conclusionDisplayContext = displayContext.withVariableDefinitions(conclusionVariableDefinitions);
        const PremiseSuggestions = () => {
          const boundVariableLists = useContext(BoundVariableLists) || [];
          return <Form.Group>
            <Form.Label><strong>Premises</strong></Form.Label>
            {selectedConclusion.possiblePremises.map(({premise, possibleMatches}, i) =>
              <Form.Group as={Form.Row} key={i}>
                <Col xs={4}>
                  <CopiableExpression expression={premise} boundVariableLists={boundVariableListsForPremises} displayContext={conclusionDisplayContext.addTermVariables(selectedConclusion.additionalVariableNames)}/>
                </Col>
                <Col>
                  <Form.Control as="select" value={this.state.selectedPremises[i][0]} onChange={(e) => this.setSelectedPremise(i, e.target.value)} readOnly={disabled}>
                    <option value="" />
                    {possibleMatches.map(({matchingPremise}, i) =>
                      <option key={i} value={i} dangerouslySetInnerHTML={{__html: renderToString(
                          <ExpressionComponent expression={matchingPremise} boundVariableLists={[...boundVariableLists, ...boundVariableListsForSubstitutions]} availableEntries={availableEntries} displayContext={displayContext} />
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
                                           onKeyDown={this.onInputKeyDown} /> :
            <BoundVariableLists.Consumer>{ boundVariableLists =>
              validValues.length === 1 ?
                <Form.Label column><CopiableExpression expression={validValues[0]} boundVariableLists={[...boundVariableLists, ...boundVariableListsForSubstitutions]} /></Form.Label> :
                <Form.Control as="select" value={getter(this.state.selectedSubstitutionValues)} onChange={e => this.setSelectedSubstitutionValue(setter, e.target.value)} readOnly={disabled}>
                  <option value="" />
                  {validValues.map(v =>
                    <option key={v.serialize()} value={v.serialize()} dangerouslySetInnerHTML={{__html: renderToString(
                        <ExpressionComponent expression={v} boundVariableLists={[...boundVariableLists, ...boundVariableListsForSubstitutions]} availableEntries={availableEntries} displayContext={displayContext}/>
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
        let showSubstitutions = (key) => {
          const variableDefinitions = selectedConclusion.variableDefinitions[key];
          return variableDefinitions.length > 0 && variableDefinitions.map(({name, arity}, index) => {
            const getter = x => x[key][index];
            const setter = (x, y) => x[key][index] = y;
            const validValues = this.getValidSubstitutionValues(getter, setter);
            const newVariableList = arity === 0 ? null :
              arity === 1 ? ["$"] :
                _.map(_.range(arity), x => "$_" + (x+1));
            return newVariableList ?
              <BoundVariableLists.AddParameters variables={newVariableList} key={`${key} ${index}`}>
                {showSubstitutionOptions(`${name}(${newVariableList.join(", ")})`, validValues, getter, setter)}
              </BoundVariableLists.AddParameters> :
              <React.Fragment key={`${key} ${index}`}>
                {showSubstitutionOptions(name, validValues, getter, setter)}
              </React.Fragment>;
          });
        };

        return <div ref={this.ref}>
          {possibleConclusions.length > 1 && <Form.Group>
            <Form.Label><strong>Choose conclusion</strong></Form.Label>
            <Form.Control as="select"
                          value={selectedConclusion ? selectedConclusionIndex : ""}
                          onChange={e => this.setSelectedConclusion(possibleConclusions[e.target.value], e.target.value)} readOnly={disabled}>
              <option value="" />
              {possibleConclusions.map(({conclusion, additionalVariableNames}, index) => {
                return <option key={index} value={index} dangerouslySetInnerHTML={{__html: renderToString(
                    <ExpressionComponent expression={conclusion} boundVariableLists={boundVariableListsForPremises} availableEntries={availableEntries} displayContext={conclusionDisplayContext.addTermVariables(additionalVariableNames)}/>
                  )}}/>
              })}
            </Form.Control>
          </Form.Group>}
          {selectedConclusion && <>
            {!hideSummary && <DisplayContext.Provider value={conclusionDisplayContext.addTermVariables(selectedConclusion.additionalVariableNames)}>
              <Form.Group>
                <ResultWithPremises premises={premiseStatements}
                                    createPremiseElement={(p, i) => <CopiableExpression key={p.serialize()} expression={p} wrapBoundVariable={wrapPremiseBoundVariable(i)}  />}
                                    result={<CopiableExpression expression={conclusionStatement} wrapBoundVariable={wrapConclusionBoundVariable}/>}/>
              </Form.Group>
            </DisplayContext.Provider>}
            {getAllRequiredPaths(selectedConclusion.variableDefinitions).length > 0 && <>
              {selectedConclusion.possiblePremises.length !== 0 && <PremiseSuggestions/>}
              <Form.Group>
                <Form.Label><strong>Substitutions</strong></Form.Label>
                {_.flatten([
                  showSubstitutions("statements"),
                  showSubstitutions("terms")
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
      }}</DisplayContext.Consumer>
    }</AvailableEntries.Consumer>
  }
}
