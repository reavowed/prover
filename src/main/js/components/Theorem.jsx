import styled from "styled-components";
import React from "react";
import {HighlightableExpression} from "./Expression";
import {InferenceSummary} from "./InferenceSummary";
import Button from "react-bootstrap/Button";
import Popover from "react-bootstrap/Popover";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";
import Modal from "react-bootstrap/Modal";
import Form from "react-bootstrap/Form";
import path from "path";
import {Parser} from "../Parser";

const ClickableDiv = props => (
  <div {...props} />
);

const ProofLine = styled(class extends React.Component {
  render() {
    const onMouseEnter = this.props.setHighlightedPremises && (() => this.props.setHighlightedPremises(this.props.step.referencedLines || []));
    const onMouseLeave = this.props.setHighlightedPremises && (() => this.props.setHighlightedPremises([]));
    const lineElement= <ClickableDiv onMouseEnter={onMouseEnter} onMouseLeave={onMouseLeave} className={this.props.className}>
      {this.props.children}
    </ClickableDiv>;

    if (this.props.popover) {
      return <OverlayTrigger trigger="click" placement="bottom" overlay={this.props.popover} rootClose>{lineElement}</OverlayTrigger>;
    } else {
      return lineElement;
    }
  }
})`
  padding-bottom: 5px;
`;

const ProofLineStatement = styled(HighlightableExpression)`
  ${ProofLine}:hover & {
    color: blue;
  }
`;

class AssumptionStep extends React.Component {
  render() {
    let {step, path, ...otherProps} = this.props;
    return <>
      <ProofLine step={step} {...otherProps}>Assume <ProofLineStatement expression={step.assumption} boundVariableLists={this.props.boundVariableLists} reference={path.join(".") + "a"} {...otherProps}/>.</ProofLine>
      <StepChildren steps={step.substeps} path={path} {...otherProps} />
    </>;
  }
}

class AssertionStep extends React.Component {
  render() {
    let {step, path, ...otherProps} = this.props;
    const inference = step.inference || step.inferenceApplication.inference;
    const popover = (
      inference && <Popover title={inference.name}>
        <InferenceSummary inference={inference} />
      </Popover>
    );
    return <ProofLine step={step} popover={popover} {...otherProps}>Then <ProofLineStatement expression={step.statement} boundVariableLists={this.props.boundVariableLists} reference={path.join(".")} {...otherProps}/>.</ProofLine>;
  }
}

class ScopedVariableStep extends React.Component {
  render() {
    let {step, path, boundVariableLists, ...otherProps} = this.props;
    return <Steps steps={step.substeps} path={path} boundVariableLists={[[step.variableName], ...boundVariableLists]} {...otherProps} />
  }
}

class TargetStep extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.showBoundVariableModal = this.showBoundVariableModal.bind(this);
    this.hideBoundVariableModal = this.hideBoundVariableModal.bind(this);
    this.updateBoundVariableName = this.updateBoundVariableName.bind(this);
    this.introduceBoundVariable = this.introduceBoundVariable.bind(this);
    this.state = {
      showBoundVariableModal: false,
      boundVariableName: props.step.statement.boundVariableNames && props.step.statement.boundVariableNames[0] || ""
    };
  }

  showBoundVariableModal() {
    this.setState({showBoundVariableModal: true})
  }

  hideBoundVariableModal() {
    this.setState({showBoundVariableModal: false})
  }

  updateBoundVariableName(event) {
    this.setState({boundVariableName: event.target.value})
  }

  introduceBoundVariable() {
    this.props.fetchForTheorem(path.join(this.props.path.join("."), "introduceBoundVariable"), {
      method: "POST",
      body: this.state.boundVariableName
    }).then(response => {
      if (response.ok) {
        return response.json();
      }
    }).then(newStep => {
        Parser.parseStep(newStep);
        this.props.updateStep(this.props.path, newStep);
    });
  }

  render() {
    let {step, path, ...otherProps} = this.props;
    let scopingStatement = _.find(window.definitions, d => d.structureType === "scoping");

    const boundVariableModal = (
        <Modal show={this.state.showBoundVariableModal} onHide={this.hideBoundVariableModal}>
          <Modal.Header closeButton><Modal.Title>Introduce bound variable</Modal.Title></Modal.Header>
          <Modal.Body>
            <Form>
              <Form.Group>
                <Form.Label>Bound variable name</Form.Label>
                <Form.Control type="text" value={this.state.boundVariableName} onChange={this.updateBoundVariableName}/>
              </Form.Group>
            </Form>
          </Modal.Body>
          <Modal.Footer>
            <Button variant="secondary" onClick={this.hideBoundVariableModal}>Close</Button>
            <Button variant="primary" onClick={this.introduceBoundVariable}>Save Changes</Button>
          </Modal.Footer>
        </Modal>
    );

    const popover = (
        <Popover title="Statement to prove">
          {scopingStatement && step.statement.definition === scopingStatement &&
            <Button variant="success" size="sm" onClick={this.showBoundVariableModal}>Introduce bound variable</Button>}
        </Popover>
    );
    return <>
      <ProofLine step={step} popover={popover} {...otherProps}>Then <ProofLineStatement expression={step.statement} reference={path.join(".")} {...otherProps}/>.</ProofLine>
      {boundVariableModal}
    </>
  }
}

class Steps extends React.Component {
  static getElementName(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
        return AssertionStep;
      case "target":
        return TargetStep;
      case "assumption":
        return AssumptionStep;
      case "scopedVariable":
        return ScopedVariableStep;
    }
  }
  static getKey(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
      case "target":
        return step.statement.serialize();
      case "assumption":
        return "assume " + step.assumption.serialize();
      case "scopedVariable":
        return "take " + step.variableName;
    }
  }
  render() {
    let {steps, className, path, ...otherProps} = this.props;
    return <div className={className}>
      {steps.map((step, index) => {
        let newProps = {
          step: step,
          path: [...path, index],
          key: Steps.getKey(step),
          ...otherProps
        };
        return React.createElement(Steps.getElementName(step), newProps);
      })}
    </div>;
  }
}

const StepChildren = styled(Steps)`
  margin-left: 20px;
`;

class Premise extends React.Component {
  render() {
    return <HighlightableExpression reference={"p" + this.props.index} highlightedPremises={this.props.highlightedPremises} expression={this.props.premise} boundVariableLists={[]}/>;
  }
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      proof: props.theorem.proof,
      highlightedPremises: []
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  createPremiseElement(premise, index) {
    return <Premise premise={premise} index={index} highlightedPremises={this.state.highlightedPremises}/>
  }

  fetchForTheorem = (childPath, options) => {
    return window.fetch(path.join(this.props.theorem.key, childPath), options);
  };

  updateStep = (path, newStep) => {
    const updateInner = (path, steps, newStep) => {
      if (path.length === 1) {
        return [...steps.slice(0, path[0]), newStep, ...steps.slice(path[0] + 1)];
      } else {
        const copiedStep = _.clone(steps[path[0]]);
        copiedStep.substeps = updateInner(path.slice(1), copiedStep.substeps, newStep);
        return [...steps.slice(0, path[0]), copiedStep, ...steps.slice(path[0] + 1)];
      }
    };
    const newProof = updateInner(path, this.state.proof, newStep);
    this.setState({proof: newProof});
  };

  render() {
    const {theorem, previousEntry, nextEntry, usages} = this.props;
    return <div className="inference">
      <div className="navigationLinks">
        {previousEntry && <a className="navigationLink float-left" href={previousEntry.key}>&laquo; {previousEntry.name}</a>}
        {nextEntry && <a className="navigationLink float-right" href={nextEntry.key}>{nextEntry.name} &raquo;</a>}
      </div>
      <div className="inferenceTitle">
        <h3>
          Theorem: {theorem.name}
        </h3>
        <div className="inferenceId">
          {theorem.id}
        </div>
      </div>

      <InferenceSummary createPremiseElement={this.createPremiseElement.bind(this)} inference={theorem} highlightedPremises={this.state.highlightedPremises}/>

      <hr/>

      <h4>Proof</h4>
      <Steps steps={this.state.proof}
             path={[]}
             boundVariableLists={[]}
             setHighlightedPremises={this.setHighlightedPremises}
             highlightedPremises={this.state.highlightedPremises}
             fetchForTheorem={this.fetchForTheorem}
             updateStep={this.updateStep}/>

      {usages.length > 0 &&
        <div>
          <hr />
          {usages.map(([usageBook, usageChapter, theorems]) =>
            <div key={usageBook.key.value + "/" + usageChapter.key.value}>
              <h6>{usageBook.title} - {usageChapter.title}</h6>
              <p>{theorems.map(theorem => <span className="usage" key={theorem.key.value}> <a className="usageLink" href={theorem.key}>{theorem.name}</a> </span>)}</p>
            </div>
          )}
        </div>
      }
    </div>
  }
}
