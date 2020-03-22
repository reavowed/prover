import _ from "lodash";
import React from "react";
import Form from "react-bootstrap/Form";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default class IntroduceName extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      namingVariableName: "",
      innerSaving: false,
      saving: false
    }
  }

  componentDidMount() {
    this.context.fetchJsonForStep(this.props.path, "suggestImmediateNamingPremises")
      .then(premiseJson => _.map(premiseJson, this.props.entryContext.parser.parsePremise))
      .then((premises) => {
        const highlightingActions = _.map(premises, p => {return {reference: p.referencedLine, action: () => this.handleImmediateNamingPremiseSelected(p.statement)}});
        this.context.setHighlightingAction(highlightingActions);
      });
  }
  componentWillUnmount() {
    this.context.clearHighlightingAction();
  }
  handleImmediateNamingPremiseSelected = (premise) => {
    return new Promise((resolve) => this.setState({saving: true}, resolve))
      .then(() => this.context.fetchJsonForStepAndReplaceWithWrapping(this.props.path, "introduceNamingFromPremise", { method: "POST", body: premise.serialize() }))
      .then(() => {
        this.context.clearHighlightingAction();
      })
      .catch(this.props.onError)
      .then(() => this.setState({saving: false}));
  };

  getInferenceSuggestions = (searchText) => {
    return this.context.fetchJsonForStep(this.props.path, `suggestNamingInferences?searchText=${encodeURIComponent(searchText)}`);
  };
  submit = (possibleInference, _, possibleConclusion, substitutions) => {
    const {namingVariableName: variableName} = this.state;
    return this.context.fetchJsonForStepAndReplaceWithWrapping(this.props.path, "introduceNaming", {
      method: "POST",
      body: {
        inferenceId: possibleInference.inference.id,
        substitutions,
        variableName
      }
    })
      .catch(this.props.onError);
  };

  render() {
    const {saving, innerSaving} = this.state;
    return <>
      <Form.Group>
        <Form.Label><strong>Variable name</strong></Form.Label>
        <Form.Control type="text"
                      readOnly={saving || innerSaving}
                      autoFocus
                      value={this.state.namingVariableName}
                      onChange={(e) => this.setState({namingVariableName: e.target.value})}/>
      </Form.Group>
      <InferenceFinder title='Select Inference for Naming'
                       getInferenceSuggestions={this.getInferenceSuggestions}
                       submit={this.submit}
                       disabled={saving}
                       onSaving={(innerSaving) => this.setState({innerSaving})}
                       hideSummary />
    </>
  }
}
