import _ from "lodash";
import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import EntryContext from "../../../../EntryContext";
import {CopiableExpression, ExpressionComponent} from "../../../../ExpressionComponent";
import ProofContext from "../../ProofContext";
import BoundVariableLists from "../BoundVariableLists";
import InferenceAutosuggest from "./components/InferenceAutosuggest";
import SuggestionDropdownElement from "./components/SuggestionDropdownElement";

export default class ExtractCurrentTarget extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      saving: false,
      selectedPremise: ""
    };
  }

  selectPremise = (event) => {
    this.setState({selectedPremise:_.find(this.props.availablePremises, p => p.serializedReference === event.target.value)});
  };

  submit = () => {
    this.setStatePromise({saving: true})
      .then(() =>
        this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "extract", {
          method: "POST",
          body: this.state.selectedPremise.statement.serialize()
        }))
      .catch(this.props.onError)
      .then(() => this.setState({saving: false}))
  };

  render() {
    const {availablePremises} = this.props;
    const {selectedPremise, saving} = this.state;

    return <BoundVariableLists.Consumer>{ boundVariableLists =>
      <EntryContext.Consumer>{ entryContext =>
        <>
          <Form.Group>
            <Form.Label><strong>Extract from premise</strong></Form.Label>
            <Form.Control as="select" value={selectedPremise ? selectedPremise.serializedReference : ""}
                          onChange={this.selectPremise}>
              <option value=""/>
              {availablePremises.map(p =>
                <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{
                  __html: renderToString(
                    <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists} entryContext={entryContext}/>
                  )
                }}/>
              )}
            </Form.Control>
          </Form.Group>
          <Button variant="primary" onClick={this.submit} disabled={saving || !this.state.selectedPremise}>
            {saving ? <span className="fas fa-spin fa-spinner"/> : "Extract"}
          </Button>
        </>
      }</EntryContext.Consumer>
    }</BoundVariableLists.Consumer>;
  }
};
