import styled from "styled-components";
import React from "react";
import {HighlightableExpression} from "./Expression";
import {InferenceSummary} from "./InferenceSummary";

const ProofLine = styled(class extends React.Component {
  render() {
    return <div onMouseEnter={() => this.props.setHighlightedPremises(this.props.step.referencedLines || [])}
                onMouseLeave={() => this.props.setHighlightedPremises([])}
                className={this.props.className}>
      {this.props.children}
    </div>;
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
    return <div>
      <ProofLine step={step} {...otherProps}>
        <span>Assume <ProofLineStatement expression={step.assumption} reference={path.join(".") + "a"} {...otherProps}/>.</span>
      </ProofLine>
      <StepChildren steps={step.substeps} path={path} {...otherProps} />
    </div>;
  }
}

class AssertionStep extends React.Component {
  render() {
    let {step, path, ...otherProps} = this.props;
    return <ProofLine step={step} {...otherProps}>
      <span>Then <ProofLineStatement expression={step.statement} reference={path.join(".")} {...otherProps}/>.</span>
    </ProofLine>;
  }
}

class Steps extends React.Component {
  static getElementName(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
      case "target":
        return AssertionStep;
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
