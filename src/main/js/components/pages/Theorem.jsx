import path from "path";
import queryString from "query-string";
import React from "react";
import Toggle from "react-bootstrap-toggle";
import Dropdown from "react-bootstrap/Dropdown";
import AvailableEntriesContext, {createAvailableEntries} from "../AvailableEntriesContext";
import {PremiseReference} from "../definitions/Reference";
import DisplaySettings, {DisplaySettingsContext} from "../DisplaySettings";
import {HighlightableExpression} from "../expressions/ExpressionComponent";
import {Inference} from "./Inference";
import Proofs from "./theorem/Proofs";
import TheoremContext from "./theorem/TheoremContext";

function Premise({statement, index}) {
  return <HighlightableExpression expression={statement} references={[new PremiseReference(index)]}/>
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    const availableEntries = createAvailableEntries(props);
    this.parser = availableEntries.parser;
    this.availableEntries = availableEntries;
    const theorem = this.parser.parseTheorem(props.theorem, props.inferences);
    this.state = {
      theorem,
      inferences: props.inferences,
      highlighting: {
        actionHighlights: [],
        staticHighlights: [],
        isActionInUse: false
      },
      disableChaining: false,
      disableAssumptionCollapse: false,
      disableShorthands: false,
      disambiguators: DisplaySettings.disambiguatorsForInferenceSummary(theorem, availableEntries)
    }
  }

  render() {
    const self = this;
    const {url} = this.props;
    const {theorem, inferences, highlighting, disableChaining, disableShorthands, disableAssumptionCollapse, disambiguators} = this.state;
    const displaySettings = new DisplaySettings(theorem.variableDefinitions, disambiguators, disableChaining, disableShorthands, disableAssumptionCollapse);

    const hashParams = queryString.parse(window.location.hash);
    const theoremContext = {
      availableEntries: this.availableEntries,
      parser: this.parser,
      variableDefinitions: theorem.variableDefinitions,
      displaySettings: displaySettings,
      inferencesToHighlight: hashParams.inferencesToHighlight ? hashParams.inferencesToHighlight.split(",") : [],
      fetchJson(subpath, options) {
        return window.fetchJson(path.join(url, subpath), options);
      },
      updateTheorem(newTheoremJson) {
        return new Promise((resolve) => {
          const newInferences = {...inferences, ...newTheoremJson.newInferences};
          const newTheorem = self.parser.parseTheorem(newTheoremJson.theorem, newInferences)
          self.setState({
            theorem: newTheorem,
            inferences: newInferences,
            disambiguators: DisplaySettings.disambiguatorsForInferenceSummary(newTheorem, self.availableEntries)
          }, () => resolve());
        })
      },
      insertSteps(proofIndex, {stepUpdates: {path, newSteps: newStepsJson}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
        const newInferences = {...inferences, ...newInferencesFromUpdate};
        const newSteps = self.parser.parseSteps(newStepsJson, newInferences);
        const stepsWithReferenceChanges = self.parser.parseStepsWithReferenceChanges(stepsWithReferenceChangesJson, newInferences);
        const newTheorem = self.state.theorem.insertSteps(proofIndex, path, newSteps)
          .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
        return self.setStatePromise({
          theorem: newTheorem,
          inferences: newInferences
        }).then(() => [path, newSteps]);
      },
      replaceStep(proofIndex, {stepUpdates: {path, newSteps: newStepsJson}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
        const newInferences = {...inferences, ...newInferencesFromUpdate};
        const newSteps = self.parser.parseSteps(newStepsJson, newInferences);
        const stepsWithReferenceChanges = self.parser.parseStepsWithReferenceChanges(stepsWithReferenceChangesJson, newInferences);
        const newTheorem = self.state.theorem.replaceStep(proofIndex, path, newSteps)
          .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
        return self.setStatePromise({
          theorem: newTheorem,
          inferences: newInferences
        }).then(() => [path, newSteps]);
      },
      insertAndReplaceSteps(proofIndex, {stepUpdates: {insertion, replacement}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
        const newInferences = {...inferences, ...newInferencesFromUpdate};
        const insertionSteps = self.parser.parseSteps(insertion.newSteps, newInferences);
        const replacementSteps = self.parser.parseSteps(replacement.newSteps, newInferences);
        const stepsWithReferenceChanges = self.parser.parseStepsWithReferenceChanges(stepsWithReferenceChangesJson, newInferences);
        const newTheorem = self.state.theorem
          .replaceStep(proofIndex, replacement.path, replacementSteps)
          .insertSteps(proofIndex, insertion.path, insertionSteps)
          .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
        return self.setStatePromise({
          theorem: newTheorem,
          inferences: newInferences
        }).then(() => [insertion.path, insertionSteps, replacement.path, replacementSteps]);
      },
      insertAndReplaceMultipleSteps(proofIndex, {stepUpdates: {insertion, replacement}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
        const newInferences = {...inferences, ...newInferencesFromUpdate};
        const insertionSteps = self.parser.parseSteps(insertion.newSteps, newInferences);
        const replacementSteps = self.parser.parseSteps(replacement.newSteps, newInferences);
        const stepsWithReferenceChanges = self.parser.parseStepsWithReferenceChanges(stepsWithReferenceChangesJson, newInferences);
        const newTheorem = self.state.theorem
          .replaceSteps(proofIndex, replacement.parentPath, replacement.startIndex, replacement.endIndex, replacementSteps)
          .insertSteps(proofIndex, insertion.path, insertionSteps)
          .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
        return self.setStatePromise({
          theorem: newTheorem,
          inferences: newInferences
        }).then(() => [insertion.path, insertionSteps, [...replacement.parentPath, replacement.startIndex], replacementSteps]);
      },
      insertAndDeleteSteps(proofIndex, {stepUpdates: {insertion, deletion}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
        const newInferences = {...inferences, ...newInferencesFromUpdate};
        const insertionSteps = self.parser.parseSteps(insertion.newSteps, newInferences);
        const stepsWithReferenceChanges = self.parser.parseStepsWithReferenceChanges(stepsWithReferenceChangesJson, newInferences);
        const newTheorem = _.reduce(
            _.range(deletion.endIndex - deletion.startIndex),
            theorem => theorem.replaceStep(proofIndex, [...deletion.parentPath, deletion.startIndex], []),
            theorem)
          .insertSteps(proofIndex, insertion.path, insertionSteps)
          .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
        return self.setStatePromise({
          theorem: newTheorem,
          inferences: newInferences
        });
      },
      setHighlighting(newHighlightedPremises, newHighlightedConclusion, proofIndex) {
        if (!highlighting.isActionInUse) {
          self.setState({highlighting: {
              ...highlighting,
              actionHighlights: _.map(newHighlightedPremises, reference => {return {reference}}),
              staticHighlights: newHighlightedConclusion ? [newHighlightedConclusion] : [],
              proofIndex: proofIndex
            }});
        }
      },
      getHighlighting(proofIndex) {
        if (_.isNumber(proofIndex) && highlighting.proofIndex === proofIndex) {
          return [highlighting.actionHighlights, highlighting.staticHighlights];
        } else {
          return [_.filter(highlighting.actionHighlights, h => (h.reference instanceof PremiseReference)), []];
        }
      },
      setHighlightingAction(actionHighlights, staticHighlights, proofIndex) {
        self.setState({
          highlighting:  {
            actionHighlights,
            staticHighlights: staticHighlights || [],
            isActionInUse: true,
            proofIndex
          }
        });
      },
      clearHighlightingAction() {
        self.setState({
          highlighting:  {
            actionHighlights: [],
            staticHighlights: [],
            isActionInUse: false,
            proofIndex: null
          }
        });
      }
    };

    const createPremiseElement = (premise, index) => {
      return <Premise statement={premise} index={index}/>
    };

    const settingsDropdown = <Dropdown className="position-static">
        <Dropdown.Toggle size="sm" className="mr-2" variant="success" id="dropdown-basic-button">Settings</Dropdown.Toggle>
        <Dropdown.Menu>
          <span className="d-flex w-100 px-4 py-1">
            <span className="flex-fill">Collapse chained steps</span>
            <Toggle onClick={() => this.setStatePromise({disableChaining: !disableChaining})}
                    active={!disableChaining}
                    size="xs"
                    className="ml-2"
                    offstyle="danger"/>
          </span>
          <span className="d-flex w-100 px-4 py-1">
            <span className="flex-fill">Collapse shorthanded expressions</span>
            <Toggle onClick={() => this.setStatePromise({disableShorthands: !disableShorthands})}
                    active={!disableShorthands}
                    size="xs"
                    className="ml-2"
                    offstyle="danger"/>
          </span>
          <span className="d-flex w-100 px-4 py-1">
            <span className="flex-fill">Collapse step assumptions</span>
            <Toggle onClick={() => this.setStatePromise({disableAssumptionCollapse: !disableAssumptionCollapse})}
                    active={!disableAssumptionCollapse}
                    size="xs"
                    className="ml-2"
                    offstyle="danger"/>
          </span>
        </Dropdown.Menu>
      </Dropdown>;

    return <AvailableEntriesContext.Provider value={this.availableEntries}>
      <TheoremContext.Provider value={theoremContext}>
        <DisplaySettingsContext.Provider value={displaySettings}>
          <Inference inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" buttons={settingsDropdown} editable {...this.props}>
            <Proofs proofs={theorem.proofs} />
          </Inference>
        </DisplaySettingsContext.Provider>
      </TheoremContext.Provider>
    </AvailableEntriesContext.Provider>;
  }
}
