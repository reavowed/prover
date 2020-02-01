import React from "react";
import ProofContext from "../../ProofContext";
import BoundVariableLists from "../BoundVariableLists";
import ConclusionChooser from "./components/ConclusionChooser";
import PremiseChooser from "./components/PremiseChooser";

export default class ProveCurrentTargetByPremise extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      selectedPremise: null,
      possibleConclusions: null
    }
  }
  setPremise = (selectedPremise) => {
    if (selectedPremise) {
      const promise = this.setStatePromise({selectedPremise})
        .then(() => this.context.fetchJsonForStep(this.props.path, `extractionSuggestions?serializedPremiseStatement=${selectedPremise.statement.serialize()}`))
        .then(this.context.parser.parsePossibleConclusions)
        .then(possibleConclusions => this.setStatePromise({possibleConclusions}));
      promise.catch(this.props.onError);
      return promise;
    } else {
      return this.setStatePromise({selectedPremise: null, possibleConclusions: null});
    }
  };
  submit = (selectedConclusion, substitutions) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "", {
      method: "PUT",
      body: {
        serializedPremiseStatement: this.state.selectedPremise.statement.serialize(),
        substitutions,
        extractionInferenceIds: selectedConclusion.extractionInferenceIds
      }
    }).catch(this.props.onError);
  };
  render () {
    const {availablePremises, entryContext} = this.props;
    const {selectedPremise, possibleConclusions, saving} = this.state;
    return <BoundVariableLists.Consumer>{boundVariableLists => <>
      <PremiseChooser premise={selectedPremise} setPremise={this.setPremise} availablePremises={availablePremises} entryContext={entryContext}/>
      {possibleConclusions && <ConclusionChooser possibleConclusions={possibleConclusions}
                                                 defaultConclusionStatement={selectedPremise.statement}
                                                 submit={this.submit}
                                                 disabled={saving}
                                                 boundVariableListsForPremises={boundVariableLists}/>}
    </>}</BoundVariableLists.Consumer>;
  }
}
