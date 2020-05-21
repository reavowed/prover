import _ from "lodash";
import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {replaceAtIndex} from "../../../../../../models/Helpers";
import EntryContext from "../../../../../EntryContext";
import {CopiableExpression, ExpressionComponent} from "../../../../../ExpressionComponent";
import {InlineTextEditor} from "../../../../../helpers/InlineTextEditor";
import InputWithShorthandReplacement from "../../../../../helpers/InputWithShorthandReplacement";
import {ResultWithPremises} from "../../../../../ResultWithPremises";
import BoundVariableLists from "../../BoundVariableLists";

function substitutionGetter(type, applicationType, name) {
  return substitutions => {
    const baseValue = substitutions[type][name] && substitutions[type][name][1];
    if (!_.isUndefined(baseValue)) {
      return baseValue;
    } else {
      return substitutions[applicationType] && substitutions[applicationType][name] && substitutions[applicationType][name][1];
    }
  };
}

function buildSubstitutionMap(requiredSubstitutions, f) {
  function callOnValues(type, applicationType) {
    return {[type]: _.fromPairs(requiredSubstitutions[type].map(([name, arity]) => [name, [arity, f(substitutionGetter(type, applicationType, name), arity)]]))};
  }
  return {
    ...callOnValues("statements", "statementApplications"),
    ...callOnValues("terms", "termApplications")
  };
}

function getAllRequiredPaths(requiredSubstitutions) {
  function getPaths(type, applicationType) {
    return _.map(requiredSubstitutions[type], ([name, ]) => substitutionGetter(type, applicationType, name));
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
    const conclusionIndexToSelect = (possibleConclusions.length > 0 && _.every(possibleConclusions.slice(1), c => _.startsWith(c.extractionInferenceIds, possibleConclusions[0].extractionInferenceIds))) ?
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
        const selectedSubstitutionValues = buildSubstitutionMap(selectedConclusion.requiredSubstitutions, () => "");
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
    this.props.submit(this.state.selectedConclusion, substitutionValues, this.state.premiseStatements, this.state.conclusionStatement);
  };

  render() {
    const {possibleConclusions, hideSummary, disabled, boundVariableListsForPremises, boundVariableListsForSubstitutions} = this.props;
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

    return <EntryContext.Consumer>{entryContext => {

      let PremiseSuggestions = () => {
        const boundVariableLists = useContext(BoundVariableLists) || [];
        return <Form.Group>
          <Form.Label><strong>Premises</strong></Form.Label>
          {selectedConclusion.possiblePremises.map(({premise, possibleMatches}, i) =>
            <Form.Group as={Form.Row} key={i}>
              <Col xs={4}>
                <CopiableExpression expression={premise} boundVariableLists={boundVariableListsForPremises}/>
              </Col>
              <Col>
                <Form.Control as="select" value={this.state.selectedPremises[i][0]} onChange={(e) => this.setSelectedPremise(i, e.target.value)} readOnly={disabled}>
                  <option value="" />
                  {possibleMatches.map(({matchingPremise}, i) =>
                    <option key={i} value={i} dangerouslySetInnerHTML={{__html: renderToString(
                        <ExpressionComponent expression={matchingPremise} boundVariableLists={[...boundVariableLists, ...boundVariableListsForSubstitutions]} entryContext={entryContext} />
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
                        <ExpressionComponent expression={v} boundVariableLists={[...boundVariableLists, ...boundVariableListsForSubstitutions]} entryContext={entryContext}/>
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
        const requiredSubstitutions = selectedConclusion.requiredSubstitutions[key];
        return requiredSubstitutions.length > 0 && requiredSubstitutions.map(([name, arity]) => {
          const getter = x => x[key][name][1];
          const setter = (x, y) => x[key][name][1] = y;
          const validValues = this.getValidSubstitutionValues(getter, setter);
          const newVariableList = arity === 0 ? null :
            arity === 1 ? ["$"] :
            _.map(_.range(arity), x => "$_" + (x+1));
          return newVariableList ?
            <BoundVariableLists.AddParameters variables={newVariableList} key={`${key} ${name}`}>
              {showSubstitutionOptions(`${name}(${newVariableList.join(", ")})`, validValues, getter, setter)}
            </BoundVariableLists.AddParameters> :
            <React.Fragment key={`${key} ${name}`}>
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
            {possibleConclusions.map(({conclusion}, index) =>
              <option key={index} value={index} dangerouslySetInnerHTML={{__html: renderToString(
                  <ExpressionComponent expression={conclusion} boundVariableLists={boundVariableListsForPremises} entryContext={entryContext}/>
                )}}/>
            )}
          </Form.Control>
        </Form.Group>}
        {selectedConclusion && <>
          {!hideSummary && <Form.Group>
            <ResultWithPremises premises={premiseStatements}
                                createPremiseElement={(p, i) => <CopiableExpression key={p.serialize()} expression={p} wrapBoundVariable={wrapPremiseBoundVariable(i)}  />}
                                result={<CopiableExpression expression={conclusionStatement} wrapBoundVariable={wrapConclusionBoundVariable}/>}/>
          </Form.Group>}
          {getAllRequiredPaths(selectedConclusion.requiredSubstitutions).length > 0 && <>
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
    }}</EntryContext.Consumer>
  }
}
