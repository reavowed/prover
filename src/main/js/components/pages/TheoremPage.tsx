import path from "path";
import queryString from "query-string";
import React, {useMemo, useState} from "react";
import Toggle from "react-bootstrap-toggle";
import Dropdown from "react-bootstrap/Dropdown";
import AvailableEntriesContext, {AvailableEntriesProps, createAvailableEntries} from "../AvailableEntriesContext";
import {PremiseReference, Reference} from "../definitions/Reference";
import DisplaySettings, {DisplaySettingsContext} from "../DisplaySettings";
import {HighlightableExpression} from "../expressions/ExpressionComponent";
import InferencePage from "./InferencePage";
import Proofs from "./theorem/Proofs";
import TheoremContext, {ActionHighlight, TheoremContextType} from "./theorem/TheoremContext";
import {Expression} from "../../models/Expression";
import {fetchJson} from "../../utils";
import _ from "lodash";
import {EntryPageProps} from "./EntryPageProps";

type PremiseProps = {
  statement: Expression
  index: number
}
function Premise({statement, index}: PremiseProps) {
  return <HighlightableExpression expression={statement} references={[new PremiseReference(index)]}/>
}
type HighlightingState = {
  actionHighlights: ActionHighlight[]
  staticHighlights: Reference[]
  proofIndex?: number
  isActionInUse: boolean
}

type TheoremPageProps = EntryPageProps & AvailableEntriesProps & {
  theorem: any
}
export default function TheoremPage(props: TheoremPageProps) {
  const {url, inferences: initialInferences = []} = props;
  const availableEntries = useMemo(() => createAvailableEntries(props), []);
  const parser = availableEntries.parser;
  const [theorem, setTheorem] = useState(() => parser.parseTheorem(props.theorem, initialInferences));
  const [inferences, setInferences] = useState(initialInferences);
  const [highlighting, setHighlighting] = useState<HighlightingState>({
    actionHighlights: [],
    staticHighlights: [],
    isActionInUse: false
  });
  const [disableChaining, setDisableChaining] = useState(false);
  const [disableAssumptionCollapse, setDisableAssumptionCollapse] = useState(false);
  const [disableShorthands, setDisableShorthands] = useState(false);
  const [disambiguators, setDisambiguators] = useState(() => DisplaySettings.disambiguatorsForInferenceSummary(theorem, availableEntries));
  const displaySettings = new DisplaySettings(theorem.variableDefinitions, disambiguators, disableChaining, disableShorthands, disableAssumptionCollapse);

  const hashParams = queryString.parse(window.location.hash);

  const theoremContext: TheoremContextType = {
    availableEntries,
    parser,
    variableDefinitions: theorem.variableDefinitions,
    displaySettings,
    inferencesToHighlight: hashParams.inferencesToHighlight ? (hashParams.inferencesToHighlight as string).split(",") : [],
    stepToHighlight: hashParams.stepToHighlight as string,
    fetchJson(subpath, options) {
      return fetchJson(path.join(url, subpath), options);
    },
    updateTheorem(newTheoremJson) {
      const newInferences = {...inferences, ...newTheoremJson.newInferences};
      const newTheorem = availableEntries.parser.parseTheorem(newTheoremJson.theorem, newInferences)
      setTheorem(newTheorem);
      setInferences(newInferences);
      setDisambiguators(DisplaySettings.disambiguatorsForInferenceSummary(newTheorem, availableEntries));
    },
    insertSteps(proofIndex, {stepUpdates: {path, newSteps: newStepsJson}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
      const newInferences = {...inferences, ...newInferencesFromUpdate};
      const newSteps = parser.parseSteps(newStepsJson, newInferences);
      const stepsWithReferenceChanges = parser.parseSteps(stepsWithReferenceChangesJson, newInferences);
      const newTheorem = theorem.insertSteps(proofIndex, path, newSteps)
        .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
      setTheorem(newTheorem);
      setInferences(newInferences);
      return [path, newSteps];
    },
    replaceStep(proofIndex, {stepUpdates: {path, newSteps: newStepsJson}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
      const newInferences = {...inferences, ...newInferencesFromUpdate};
      const newSteps = parser.parseSteps(newStepsJson, newInferences);
      const stepsWithReferenceChanges = parser.parseSteps(stepsWithReferenceChangesJson, newInferences);
      const newTheorem = theorem.replaceStep(proofIndex, path, newSteps)
        .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
      setTheorem(newTheorem);
      setInferences(newInferences);
      return [path, newSteps];
    },
    insertAndReplaceSteps(proofIndex, {stepUpdates: {insertion, replacement}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
      const newInferences = {...inferences, ...newInferencesFromUpdate};
      const insertionSteps = parser.parseSteps(insertion.newSteps, newInferences);
      const replacementSteps = parser.parseSteps(replacement.newSteps, newInferences);
      const stepsWithReferenceChanges = parser.parseSteps(stepsWithReferenceChangesJson, newInferences);
      const newTheorem = theorem
        .replaceStep(proofIndex, replacement.path, replacementSteps)
        .insertSteps(proofIndex, insertion.path, insertionSteps)
        .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
      setTheorem(newTheorem);
      setInferences(newInferences);
      return [insertion.path, insertionSteps, replacement.path, replacementSteps];
    },
    insertAndReplaceMultipleSteps(proofIndex, {stepUpdates: {insertion, replacement}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
      const newInferences = {...inferences, ...newInferencesFromUpdate};
      const insertionSteps = parser.parseSteps(insertion.newSteps, newInferences);
      const replacementSteps = parser.parseSteps(replacement.newSteps, newInferences);
      const stepsWithReferenceChanges = parser.parseSteps(stepsWithReferenceChangesJson, newInferences);
      const newTheorem = theorem
        .replaceSteps(proofIndex, replacement.parentPath, replacement.startIndex, replacement.endIndex, replacementSteps)
        .insertSteps(proofIndex, insertion.path, insertionSteps)
        .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
      setTheorem(newTheorem);
      setInferences(newInferences);
      return [insertion.path, insertionSteps, [...replacement.parentPath, replacement.startIndex], replacementSteps];
    },
    insertAndDeleteSteps(proofIndex, {stepUpdates: {insertion, deletion}, newInferences: newInferencesFromUpdate, stepsWithReferenceChanges: stepsWithReferenceChangesJson}) {
      const newInferences = {...inferences, ...newInferencesFromUpdate};
      const insertionSteps = parser.parseSteps(insertion.newSteps, newInferences);
      const stepsWithReferenceChanges = parser.parseSteps(stepsWithReferenceChangesJson, newInferences);
      const newTheorem = _.reduce(
          _.range(deletion.endIndex - deletion.startIndex),
          theorem => theorem.replaceStep(proofIndex, [...deletion.parentPath, deletion.startIndex], []),
          theorem)
        .insertSteps(proofIndex, insertion.path, insertionSteps)
        .updateStepsWithReferenceChanges(proofIndex, stepsWithReferenceChanges);
      setTheorem(newTheorem);
      setInferences(newInferences);
    },
    setHighlighting(newHighlightedPremises, newHighlightedConclusion, proofIndex) {
      if (!highlighting.isActionInUse) {
        setHighlighting({
          ...highlighting,
          actionHighlights: _.map(newHighlightedPremises, reference => {return {reference}}),
          staticHighlights: newHighlightedConclusion ? [newHighlightedConclusion] : [],
          proofIndex: proofIndex
        });
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
      setHighlighting({
        actionHighlights,
        staticHighlights: staticHighlights || [],
        isActionInUse: true,
        proofIndex
      });
    },
    clearHighlightingAction() {
      setHighlighting({
        actionHighlights: [],
        staticHighlights: [],
        isActionInUse: false
      });
    }
  };

  const createPremiseElement = (premise: Expression, index: number) => {
    return <Premise statement={premise} index={index}/>
  };

  const settingsDropdown = <Dropdown className="position-static">
      <Dropdown.Toggle size="sm" className="mr-2" variant="success" id="dropdown-basic-button">Settings</Dropdown.Toggle>
      <Dropdown.Menu>
        <span className="d-flex w-100 px-4 py-1">
          <span className="flex-fill">Collapse chained steps</span>
          <Toggle onClick={() => setDisableChaining(v => !v)}
                  active={!disableChaining}
                  size="xs"
                  className="ml-2"
                  offstyle="danger"/>
        </span>
        <span className="d-flex w-100 px-4 py-1">
          <span className="flex-fill">Collapse shorthanded expressions</span>
          <Toggle onClick={() => setDisableShorthands(v => !v)}
                  active={!disableShorthands}
                  size="xs"
                  className="ml-2"
                  offstyle="danger"/>
        </span>
        <span className="d-flex w-100 px-4 py-1">
          <span className="flex-fill">Collapse step assumptions</span>
          <Toggle onClick={() => setDisableAssumptionCollapse(v => !v)}
                  active={!disableAssumptionCollapse}
                  size="xs"
                  className="ml-2"
                  offstyle="danger"/>
        </span>
      </Dropdown.Menu>
    </Dropdown>;

  return <AvailableEntriesContext.Provider value={availableEntries}>
    <TheoremContext.Provider value={theoremContext}>
      <DisplaySettingsContext.Provider value={displaySettings}>
        <InferencePage inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" buttons={settingsDropdown} editable {...props}>
          <Proofs proofs={theorem.proofs} />
        </InferencePage>
      </DisplaySettingsContext.Provider>
    </TheoremContext.Provider>
  </AvailableEntriesContext.Provider>;
}
