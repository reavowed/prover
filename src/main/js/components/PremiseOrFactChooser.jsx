import _ from "lodash";
import React from "react";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {connect} from "react-redux";
import {Parser} from "../Parser";
import {CopiableExpression} from "./ExpressionComponent";
import InferenceAutosuggest from "./InferenceAutosuggest";
import SuggestionDropdownElement from "./SuggestionDropdownElement";
import ProofContext from "./theorem/ProofContext";
import {FetchJsonForStep} from "./theorem/TheoremStore";

export default connect()(class PremiseOrFactChooser extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      autosuggestValue: "",
      facts: [],
      selectedPremise: ""
    };
  }

  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({ value }) => {
    this.props.dispatch(FetchJsonForStep(this.context.proofIndex, this.props.path, `suggestFacts?searchText=${value}`))
      .then(factsJson => {
        if (this.state.autosuggestValue === value) {
          this.setState({facts: _.map(factsJson, Parser.parseInference)});
        }
      });
  };
  onSuggestionsClearRequested = () => {
    this.setState({facts: []})
  };
  onSuggestionSelected = (event, {suggestion}) => {
    this.props.onFactSelected(suggestion);
  };

  selectPremise = (event) => {
    this.setState({selectedPremise: event.target.value});
    this.props.onPremiseSelected(_.find(this.props.availablePremises, p => p.serializedReference === event.target.value));
  };

  render() {
    const {availablePremises, boundVariableLists, title} = this.props;
    const {selectedPremise} = this.state;

    let getSuggestionValue = s => s.name;
    let renderSuggestion = s => <SuggestionDropdownElement
      mainElement={getSuggestionValue(s)}
      hoverElement={<CopiableExpression expression={s.conclusion} boundVariableLists={[]} />} />;

    return <>
      <Form.Group>
        <Form.Label><strong>{title} fact</strong></Form.Label>
        <InferenceAutosuggest
          autoFocus
          value={this.state.autosuggestValue}
          onValueChange={this.onAutosuggestChange}
          suggestions={this.state.facts}
          getSuggestionValue={getSuggestionValue}
          renderSuggestion={renderSuggestion}
          onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
          onSuggestionsClearRequested={this.onSuggestionsClearRequested}
          onSuggestionSelected={this.onSuggestionSelected} />
      </Form.Group>
      <Form.Group>
        <Form.Label><strong>{title} premise</strong></Form.Label>
        <Form.Control as="select" value={selectedPremise ? selectedPremise.serializedReference : ""} onChange={this.selectPremise}>
          <option value="" />
          {availablePremises.map(p =>
            <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{__html: renderToString(
                <CopiableExpression expression={p.statement} boundVariableLists={boundVariableLists} />
              )}}/>
          )}
        </Form.Control>
      </Form.Group>
    </>;
  }
});
