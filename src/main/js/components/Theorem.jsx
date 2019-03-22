import styled from "styled-components";
import React from "react";
import {HighlightableExpression} from "./Expression";
import {InferenceSummary} from "./InferenceSummary";
import Button from "react-bootstrap/Button";
import Popover from "react-bootstrap/Popover";
import OverlayTrigger from "react-bootstrap/OverlayTrigger";

const ClickableDiv = props => (
  <div {...props} />
);

const ProofLine = styled(class extends React.Component {
  render() {
    const lineElement= <ClickableDiv onMouseEnter={() => this.props.setHighlightedPremises(this.props.step.referencedLines || [])}
                onMouseLeave={() => this.props.setHighlightedPremises([])}
                className={this.props.className}>
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
      <ProofLine step={step} {...otherProps}>Assume <ProofLineStatement expression={step.assumption} reference={path.join(".") + "a"} {...otherProps}/>.</ProofLine>
      <StepChildren steps={step.substeps} path={path} {...otherProps} />
    </>;
  }
}

class AssertionStep extends React.Component {
  render() {
    let {step, path, ...otherProps} = this.props;
    const popover = (
      <Popover title={step.inference.name}>
        <InferenceSummary inference={step.inference} />
      </Popover>
    );
    return <ProofLine step={step} popover={popover} {...otherProps}>Then <ProofLineStatement expression={step.statement} reference={path.join(".")} {...otherProps}/>.</ProofLine>;
  }
}

class TargetStep extends React.Component {
  render() {
    let {step, path, ...otherProps} = this.props;
    let scopingStatement = _.find(window.definitions, d => d.structureType === "scoping");
    const popover = (
      <Popover title="Statement to prove">
        {scopingStatement && step.statement.definition === scopingStatement && <Button variant="success" size="sm">Introduce bound variable</Button>}
      </Popover>
    );
    return <ProofLine step={step} popover={popover} {...otherProps}>Then <ProofLineStatement expression={step.statement} reference={path.join(".")} {...otherProps}/>.</ProofLine>
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
    }
  }
  static getKeyStatement(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
      case "target":
        return step.statement;
      case "assumption":
        return step.assumption;
    }
  }
  render() {
    let {steps, className, path, ...otherProps} = this.props;
    return <div className={className}>
      {steps.map((step, index) => {
        let newProps = {
          step: step,
          path: [...path, index],
          key: step.type + " " + Steps.getKeyStatement(step).serialize(),
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
    return <HighlightableExpression reference={"p" + this.props.index} highlightedPremises={this.props.highlightedPremises} expression={this.props.premise}/>;
  }
}

export class Theorem extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      highlightedPremises: []
    }
  }

  setHighlightedPremises = (premises) => {
    this.setState({highlightedPremises: premises});
  };

  createPremiseElement(premise, index) {
    return <Premise premise={premise} index={index} highlightedPremises={this.state.highlightedPremises}/>
  }

  render() {
    const {theorem, previousEntry, nextEntry, usages} = this.props;
    let {proof} = theorem;
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
      <Steps steps={proof} path={[]} setHighlightedPremises={this.setHighlightedPremises} highlightedPremises={this.state.highlightedPremises} />

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
