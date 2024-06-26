import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import FlexRow from "../../../../FlexRow";
import InputWithShorthandReplacement from "../../../../helpers/InputWithShorthandReplacement";
import ProofContext from "../../ProofContext";

export default class AddTargetManually extends React.Component {
  static contextType = ProofContext;

  constructor(props) {
    super(props);
    this.state = {
      saving: false,
      targetStatement: ""
    }
  }

  addTarget = () => {
    return new Promise(((resolve) => this.setState({saving: true}, resolve)))
      .then(() => this.context.fetchJsonForStepAndInsert(this.props.path, "target", {
        method: "POST",
        body: this.state.targetStatement
      }))
      .then(this.props.onCancel)
      .catch(this.props.onError)
      .then(() => this.setState({saving: false}));
  };

  render() {
    const {saving} = this.state;
    const onKeyUp = (event) => {
      if (event.key === "Enter") {
        this.addTarget();
      }
    };
    return <>
      <Form.Group>
        <Form.Label><strong>Target</strong></Form.Label>
        <FlexRow>
          <FlexRow.Grow>
            <InputWithShorthandReplacement autoFocus
                                           readOnly={saving}
                                           value={this.state.targetStatement}
                                           onChange={(targetStatement, callback) => this.setState({targetStatement}, callback)}
                                           onKeyUp={onKeyUp} />
          </FlexRow.Grow>
          <Button size="sm" className="ml-1" onClick={this.addTarget} disabled={saving}>{saving ? <span className="fas fa-spin fa-spinner"/> : "Add"}</Button>
        </FlexRow>
      </Form.Group>
    </>
  }
}
