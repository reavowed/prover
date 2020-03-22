import _ from "lodash";
import React, {useContext, useState} from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {ExpressionComponent} from "../../../../ExpressionComponent";
import ProofContext from "../../ProofContext";
import BoundVariableLists from "../BoundVariableLists";
import Rewriter from "./components/Rewriter";

export default class RewriteCurrentTargetByDefinition extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      saving: false,
      selectedPremise: ""
    };
  }
  submit = () => {
    return this.context.fetchJsonForStepAndReplace(this.props.path, "rewriteDefinition", {
      method: "POST",
      body: this.state.selectedPremise.statement.serialize()
    })
      .then(this.props.onCancel)
      .catch(this.props.onError);
  };
  render() {
    const {availablePremises, entryContext} = this.props;
    const {saving, selectedPremise} = this.state;

    return <BoundVariableLists.Consumer>{boundVariableLists =>
      <>
        <Form.Group>
          <Form.Label><strong>Choose premise</strong></Form.Label>
          <Form.Control as="select" autoFocus value={selectedPremise ? selectedPremise.serializedReference : ""}
                        onChange={e => this.setState({selectedPremise: _.find(availablePremises, p => p.serializedReference === e.target.value)})}>
            <option value=""/>
            {availablePremises.map(p =>
              <option key={p.serializedReference} value={p.serializedReference} dangerouslySetInnerHTML={{
                __html: renderToString(
                  <ExpressionComponent expression={p.statement} boundVariableLists={boundVariableLists}
                                       entryContext={entryContext}/>
                )
              }}/>
            )}
          </Form.Control>
        </Form.Group>
        <Button variant="primary" onClick={this.submit} disabled={saving || !this.state.selectedPremise}>
          {saving ? <span className="fas fa-spin fa-spinner"/> : "Rewrite"}
        </Button>
      </>
    }</BoundVariableLists.Consumer>;
  }
}
