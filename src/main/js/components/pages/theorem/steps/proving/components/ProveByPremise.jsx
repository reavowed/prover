import React from "react";
import ProofContext from "../../../ProofContext";
import BoundVariableLists from "../../BoundVariableLists";
import ConclusionChooser from "./ConclusionChooser";
import PremiseChooser from "./PremiseChooser";

export default class ProveByPremise extends React.Component {
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
        .then(() => this.props.fetchPossibleConclusions(selectedPremise.statement))
        .then(this.context.parser.parsePossibleConclusions)
        .then(possibleConclusions => this.setStatePromise({possibleConclusions}));
      promise.catch(this.props.onError);
      return promise;
    } else {
      return this.setStatePromise({selectedPremise: null, possibleConclusions: null});
    }
  };
  submit = (selectedConclusion, substitutions, premiseStatements, conclusionStatement) => {
    return this.props.submit(this.state.selectedPremise.statement, substitutions, selectedConclusion, premiseStatements, conclusionStatement);
  };
  render () {
    const {availablePremises, entryContext} = this.props;
    const {selectedPremise, possibleConclusions, saving} = this.state;
    return <BoundVariableLists.Consumer>{boundVariableLists => <>
      <PremiseChooser premise={selectedPremise} setPremise={this.setPremise} availablePremises={availablePremises} entryContext={entryContext} autoFocus />
      {possibleConclusions && <ConclusionChooser possibleConclusions={possibleConclusions}
                                                 defaultConclusionStatement={selectedPremise.statement}
                                                 submit={this.submit}
                                                 disabled={saving}
                                                 boundVariableListsForPremises={boundVariableLists}
                                                 boundVariableListsForSubstitutions={[]}/>}
    </>}</BoundVariableLists.Consumer>;
  }
}
