import path from "path";
import React from "react";
import _ from "lodash";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Modal from "react-bootstrap/Modal";
import {Expression} from "../models/Expression";
import {Step} from "../models/Step";
import {HighlightableExpression} from "./ExpressionComponent";
import {Inference} from "./Inference";
import {Steps} from "./steps/Steps";
import {Parser} from "../Parser";

class Premise extends React.Component {
  render() {
    return <HighlightableExpression reference={{premiseIndex: this.props.index}} highlighting={this.props.highlighting} expression={this.props.premise} boundVariableLists={[]}/>;
  }
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.targetInputRef = React.createRef();
    this.state = {
      theorem: this.parseTheorem(props.theorem),
      addingTarget: false,
      targetToAdd: '',
      highlightedPremises: [],
      highlightedConclusion: null
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  setHighlightedConclusion = (conclusion) => {
    this.setState({highlightedConclusion: conclusion});
  };

  fetchJsonForStep = (stepPath, childPath, options) => {
    const combinedPath = path.join(this.props.url, stepPath.join("."), childPath) + (childPath === "" ? "/" : "");
    return window.fetch(combinedPath, options)
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.statusText;
        }
      });
  };

  parseTheorem = (theoremJson) => {
    return {
      name: theoremJson.name,
      id: theoremJson.id,
      key: theoremJson.key,
      premises: theoremJson.premises.map(Expression.parseFromJson),
      conclusion: Expression.parseFromJson(theoremJson.conclusion),
      proof: Step.parseFromJson(theoremJson.proof)
    };
  };

  updateTheorem = (result) => {
    _.merge(window.inferences, result.newInferences);
    const theorem = this.parseTheorem(result.theorem);
    this.setState({theorem: theorem});
  };

  hideTargetModal = () => this.setState({addingTarget: false});

  addTarget = () => {
    window.fetch(this.props.url + "/target", {method: "POST", body: this.state.targetToAdd})
      .then(response => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.statusText;
        }
      })
      .then(result => {
        this.hideTargetModal();
        this.updateTheorem(result)
      });
  };

  render() {
    const {theorem} = this.state;
    const highlighting = {
      highlightedPremises: this.state.highlightedPremises,
      highlightedConclusion: this.state.highlightedConclusion,
      setHighlightedPremises: this.setHighlightedPremises,
      setHighlightedConclusion: this.setHighlightedConclusion
    };
    const apiService = {
      fetchJsonForStep: this.fetchJsonForStep,
      updateTheorem: this.updateTheorem
    };
    const createPremiseElement = (premise, index) => {
      return <Premise premise={premise} index={index} highlighting={highlighting}/>
    };

    const targetModal = <Modal show={this.state.addingTarget} onHide={this.hideTargetModal} onEntered={() => {
      this.targetInputRef.current.focus();
      this.targetInputRef.current.select();
    }}>
      <Modal.Header closeButton><Modal.Title>Add target statement</Modal.Title></Modal.Header>
      <Modal.Body>
        <Form.Group>
          <Form.Control type="text"
                        value={this.state.targetToAdd}
                        onChange={e => this.setState({targetToAdd: Parser.replaceShorthands(e.target.value)})}
                        onKeyUp={(event) => {
                          if (event.keyCode === 13) {
                            this.addTarget();
                          }
                          event.preventDefault();
                          event.stopPropagation();
                        }}
                        ref={this.targetInputRef}/>
        </Form.Group>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={this.hideTargetModal}>Close</Button>
        <Button variant="primary" onClick={this.addTarget}>Save Changes</Button>
      </Modal.Footer>
    </Modal>;

    return <Inference inference={theorem} createPremiseElement={createPremiseElement} title="Theorem" {...this.props}>
      <hr/>
      <h4>
        Proof
        <Button onClick={() => this.setState({addingTarget: true})} variant="success" size="sm" className="ml-2">Add target</Button>
      </h4>
      <Steps steps={theorem.proof}
             path={[]}
             boundVariableLists={[]}
             highlighting={highlighting}
             apiService={apiService}/>
      {targetModal}
    </Inference>;
  }
}
