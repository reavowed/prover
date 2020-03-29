import React from "react";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import {renderToString} from "react-dom/server";
import {ExpressionComponent} from "../../../../ExpressionComponent";
import {FlexRow} from "../../../../FlexRow";
import InputWithShorthandReplacement from "../../../../helpers/InputWithShorthandReplacement";
import ProofContext from "../../ProofContext";

export default class AddChainedTarget extends React.Component {
  static contextType = ProofContext;

  constructor(props) {
    super(props);
    this.state = {
      joiners: null,
      chosenJoinerIndex: -1,
      saving: false,
      targetStatement: ""
    }
  }

  componentDidMount() {
    this.context.fetchJsonForStep(this.props.path, "chainedTargetJoiners")
      .then(joiners => this.setStatePromise({joiners}))
  }

  addTarget = () => {
    const joiners = this.state.joiners[this.state.chosenJoinerIndex];
    return new Promise(((resolve) => this.setState({saving: true}, resolve)))
      .then(() => this.context.fetchJsonForStepAndInsertAndReplaceMultiple(this.props.path, "chainedTarget", {
        method: "POST",
        body: {
          serializedExpression: this.state.targetStatement,
          leftJoiner: joiners[0],
          rightJoiner: joiners[1]
        }
      }))
      .then(this.props.onCancel)
      .catch(this.props.onError)
      .then(() => this.setState({saving: false}));
  };

  render() {
    const {saving, joiners, chosenJoinerIndex, targetStatement} = this.state;
    const onKeyUp = (event) => {
      if (event.key === "Enter") {
        this.addTarget();
      }
    };
    const readyToSave = targetStatement && targetStatement.trim().length && chosenJoinerIndex;
    return <>
      {!joiners && <span className="fas fa-spin fa-spinner text-center mt-4 mb-4"/>}
      {joiners && <>
        <Form.Group>
          <Form.Label><strong>Transitive Target</strong></Form.Label>
          <InputWithShorthandReplacement autoFocus
                                         readOnly={saving}
                                         value={this.state.targetStatement}
                                         onChange={(targetStatement, callback) => this.setState({targetStatement}, callback)}
                                         onKeyUp={onKeyUp} />
        </Form.Group>
        <Form.Group>
          <Form.Label><strong>Joiners</strong></Form.Label>
          <Form.Control as="select" value={chosenJoinerIndex} onChange={e => this.setState({chosenJoinerIndex: e.target.value})} readOnly={saving}>
            <option value="-1" />
            {joiners.map((v, i) => <option key={i} value={i}>{v[0]}, {v[1]}</option>)}
          </Form.Control>
        </Form.Group>

      <Button size="sm" className="ml-1" onClick={this.addTarget} disabled={saving || !readyToSave}>{saving ? <span className="fas fa-spin fa-spinner"/> : "Add"}</Button>
      </>}
    </>
  }
}
