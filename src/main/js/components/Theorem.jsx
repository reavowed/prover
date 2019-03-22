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
import {formatHtml, Parser} from "../Parser";
import {FindInferenceModal} from "./Modals";

const ClickableDiv = props => (
  <div {...props} />
);

const ProofLine = styled(class ProofLine extends React.Component {
  render() {
    const {setHighlightedPremises, referencedLines, className, children, popover} = this.props;

    const onMouseEnter = referencedLines && setHighlightedPremises && (() => setHighlightedPremises(referencedLines));
    const onMouseLeave = referencedLines && setHighlightedPremises && (() => setHighlightedPremises([]));
    const lineElement= <ClickableDiv onMouseEnter={onMouseEnter} onMouseLeave={onMouseLeave} className={className}>
      {children}
    </ClickableDiv>;

    if (popover) {
      return <OverlayTrigger trigger="click" placement="bottom" overlay={popover} rootClose>{lineElement}</OverlayTrigger>;
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

class DeductionStep extends React.Component {
  render() {
    let {step, path, ...otherProps} = this.props;
    return <>
      <ProofLine>Assume <HighlightableExpression expression={step.assumption} boundVariableLists={this.props.boundVariableLists} reference={path.join(".") + "a"} {...otherProps}/>.</ProofLine>
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
    return <ProofLine referencedLines={step.referencedLines} popover={popover} {...otherProps}>
      Then <ProofLineStatement expression={step.statement} boundVariableLists={this.props.boundVariableLists} reference={path.join(".")} {...otherProps}/>.
    </ProofLine>;
  }
}

class ScopedVariableStep extends React.Component {
  render() {
    let {step, path, boundVariableLists, ...otherProps} = this.props;
    return <Steps steps={step.substeps} path={path} boundVariableLists={[[step.variableName], ...boundVariableLists]} {...otherProps} />
  }
}

class NamingStep extends React.Component {
  render() {
    const {step, path, boundVariableLists, ...otherProps} = this.props;
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine referencedLines={step.finalInferenceApplication.referencedLines} {...otherProps}>
        Let {formatHtml(step.variableName)} be such that <ProofLineStatement expression={step.assumption} boundVariableLists={innerBoundVariableLists} reference={path.join(".") + "a"} {...otherProps}/>.
      </ProofLine>
      <Steps steps={step.substeps} path={path} boundVariableLists={innerBoundVariableLists} {...otherProps} />
    </>;
  }
}

class TargetStep extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.state = {
      showBoundVariableModal: false,
      showFindInferenceModal: false,
      boundVariableName: props.step.statement.boundVariableNames && props.step.statement.boundVariableNames[0] || ""
    };
  }

  showBoundVariableModal = () => {
    this.setState({showBoundVariableModal: true})
  };

  hideBoundVariableModal = () => {
    this.setState({showBoundVariableModal: false})
  };

  updateBoundVariableName = (event) => {
    this.setState({boundVariableName: event.target.value})
  };

  introduceBoundVariable = () => {
    this.props.fetchForStep(this.props.path, "introduceBoundVariable", {
      method: "POST",
      body: this.state.boundVariableName
    }).then(response => {
      if (response.ok) {
        return response.json();
      }
    }).then(this.props.updateTheorem);
  };

  showFindInferenceModal = () => {
    this.setState({showFindInferenceModal: true})
  };

  hideFindInferenceModal = () => {
    this.setState({showFindInferenceModal: false})
  };

  findInferences = (searchText) => {
    return this.props.fetchForStep(this.props.path, `suggestInferences?searchText=${searchText}`)
  };

  introduceDeduction = () => {
    this.props.fetchForStep(this.props.path, "introduceDeduction", {
      method: "POST"
    }).then(response => {
      if (response.ok) {
        return response.json();
      }
    }).then(this.props.updateTheorem);
  };

  proveWithInference = (inferenceId, substitutions) => {
    this.props.fetchForStep(this.props.path, {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions})
    }).then(response => {
      if (response.ok) {
        return response.json();
      }
    }).then(newStep => {
      console.log(newStep);
    });
  };

  render() {
    let {step, path, ...otherProps} = this.props;
    let scopingStatement = _.find(window.definitions, d => d.structureType === "scoping");
    let deductionStatement = _.find(window.definitions, d => d.structureType === "deduction");

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
          {deductionStatement && step.statement.definition === deductionStatement &&
            <Button variant="success" size="sm" onClick={this.introduceDeduction}>Introduce deduction</Button>}
          <Button variant="success" size="sm" onClick={this.showFindInferenceModal}>Find inference</Button>
        </Popover>
    );
    return <>
      <ProofLine step={step} popover={popover} {...otherProps}>Then <ProofLineStatement expression={step.statement} reference={path.join(".")} {...otherProps}/>.</ProofLine>
      {boundVariableModal}
      {<FindInferenceModal show={this.state.showFindInferenceModal} onHide={this.hideFindInferenceModal} onSubmit={this.proveWithInference} findInferences={this.findInferences} {...otherProps} />}
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
      case "deduction":
        return DeductionStep;
      case "scopedVariable":
        return ScopedVariableStep;
      case "naming":
        return NamingStep;
    }
  }
  static getKey(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
      case "target":
        return step.statement.serialize();
      case "deduction":
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
      theorem: props.theorem,
      highlightedPremises: []
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  createPremiseElement(premise, index) {
    return <Premise premise={premise} index={index} highlightedPremises={this.state.highlightedPremises}/>
  }

  fetchForStep = (stepPath, childPath, options) => {
    if (_.isObject(childPath)) {
      options = childPath;
      childPath = "";
    }
    const combinedPath = path.join(this.state.theorem.key, stepPath.join("."), childPath) + (childPath === "" ? "/" : "");
    return window.fetch(combinedPath, options);
  };

  updateTheorem = (theoremJSON) => {
    const theorem = Parser.parseTheorem(theoremJSON);
    this.setState({ theorem: theorem });
  };

  render() {
    const {previousEntry, nextEntry, usages} = this.props;
    const {theorem} = this.state;
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
      <Steps steps={theorem.proof}
             path={[]}
             boundVariableLists={[]}
             setHighlightedPremises={this.setHighlightedPremises}
             highlightedPremises={this.state.highlightedPremises}
             fetchForStep={this.fetchForStep}
             updateTheorem={this.updateTheorem}/>

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
