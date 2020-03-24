import path from "path";
import React from "react";
import {PremiseReference} from "../../models/Step";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {HighlightableExpression} from "../ExpressionComponent";
import HashParamsContext from "../HashParamsContext";
import {Inference} from "./Inference";
import Proofs from "./theorem/Proofs";
import TheoremContext from "./theorem/TheoremContext";
import queryString from "query-string";

function Premise({statement, index}) {
  return <HighlightableExpression expression={statement} references={[new PremiseReference(index)]}/>
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.parser = new Parser(props.definitions, props.typeDefinitions);
    this.entryContext = EntryContext.create(this.parser, props.definitions, props.typeDefinitions, props.definitionShorthands, props.displayShorthands, props.inferences, props.binaryRelations);

    this.state = {
      theorem: this.parser.parseTheorem(props.theorem, props.inferences),
      inferences: props.inferences,
      highlighting: {
        actionHighlights: [],
        staticHighlights: [],
        isActionInUse: false
      },
      disableChaining: false
    }
  }

  onKeyDown = (event) => {
    if (event.target instanceof HTMLTextAreaElement || event.target instanceof HTMLInputElement) {
      return;
    }
    if (event.key === "c") {
      this.setState({disableChaining: !this.state.disableChaining});
    }
  };

  componentDidMount() {
    document.body.addEventListener('keydown', this.onKeyDown);
  }
  componentWillUnmount() {
    document.body.removeEventListener('keydown', this.onKeyDown);
  }


  render() {
    const self = this;
    const {url} = this.props;
    const {theorem, inferences, highlighting, disableChaining} = this.state;

    const theoremContext = {
      entryContext: this.entryContext,
      parser: this.parser,
      disableChaining,
      fetchJson(subpath,  options) {
        return window.fetchJson(path.join(url, subpath), options);
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

    const rawHashParams = queryString.parse(window.location.hash);
    const hashParams = {};
    if (rawHashParams.inferencesToHighlight) {
      hashParams.inferencesToHighlight = rawHashParams.inferencesToHighlight.split(",")
    }

    return <HashParamsContext.Provider value={hashParams}>
      <EntryContext.Provider value={this.entryContext}>
        <TheoremContext.Provider value={theoremContext}>
          <Inference inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" {...this.props}>
            <Proofs proofs={theorem.proofs} />
          </Inference>
        </TheoremContext.Provider>
      </EntryContext.Provider>
    </HashParamsContext.Provider>;
  }
}
