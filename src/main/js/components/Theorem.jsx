import styled from "styled-components";
import React from "react";
import {Expression, formatHtml, HighlightableStatement} from "./Expression";
import {InferenceSummary} from "./InferenceSummary";
import Button from "react-bootstrap/Button";
import Popover from "react-bootstrap/Popover";
import Overlay from "react-bootstrap/Overlay";
import Modal from "react-bootstrap/Modal";
import Form from "react-bootstrap/Form";
import path from "path";
import {Parser} from "../Parser";
import {FindInferenceModal} from "./Modals";
import DropdownButton from "react-bootstrap/DropdownButton";
import Dropdown from "react-bootstrap/Dropdown";

const ProofLine = styled(class ProofLine extends React.Component {
  constructor(...args) {
    super(...args);
    this.attachRef = target => this.setState({ target });
    this.state = {
      showPopover: false,
    };
  }
  showPopover = () => {
    this.setState({showPopover: true})
  };
  hidePopover = () => {
    this.setState({showPopover: false})
  };
  render() {
    const {setHighlightedPremises, referencedLines, className, children, popover, onShowPopover} = this.props;

    const onMouseEnter = referencedLines && setHighlightedPremises && (() => setHighlightedPremises(referencedLines));
    const onMouseLeave = referencedLines && setHighlightedPremises && (() => setHighlightedPremises([]));
    const lineElement= <div onMouseEnter={onMouseEnter} onMouseLeave={onMouseLeave} onClick={this.showPopover} className={className} ref={this.attachRef}>
      {children}
    </div>;

    if (popover) {
      return <>
        {lineElement}
        <Overlay target={this.state.target} show={this.state.showPopover} onEnter={onShowPopover} onHide={this.hidePopover} rootClose placement="bottom-start">
          {popover}
        </Overlay>
      </>
    } else {
      return lineElement;
    }
  }
})`
  padding-bottom: 5px;
`;

const ProofLineStatement = styled(HighlightableStatement)`
  ${ProofLine}:hover & {
    color: blue;
  }
`;

class DeductionStep extends React.Component {
  render() {
    let {step, path, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    return <>
      <ProofLine>Assume <HighlightableStatement statement={step.assumption} boundVariableLists={this.props.boundVariableLists} references={[...additionalReferences, reference, reference + "a"]} {...otherProps}/>.</ProofLine>
      <StepChildren steps={step.substeps} path={path} referencesForLastStep={referencesForLastStep} {...otherProps} />
    </>;
  }
}

const PremiseChildren = styled.div`
  margin-left: 20px;
`;

class AssertionStep extends React.Component {
  constructor(...args) {
    super(...args);
    this.state = {
      premiseOptions: {}
    };
  }

  applyExpansion = (premisePath, expansionId) => {
    this.props.fetchForStep(this.props.path, `premises/${premisePath.join(".")}/rearrangement`, {
      method: "POST",
      body: expansionId
    }).then(this.props.updateTheorem);
  };

  renderPremise(premise, path, boundVariableLists) {
    switch (premise.type) {
      case "pending":
        const options = _.find(this.state.premiseOptions, option => _.isEqual(option.path, path)) || {};
        return <div>
          <Expression expression={premise.statement} boundVariableLists={boundVariableLists}/>
          {options.expansions && <DropdownButton title="Expansions" size="sm">
            {options.expansions.map(e => <Dropdown.Item key={e.id} onClick={() => this.applyExpansion(path, e.id)}>{e.name}</Dropdown.Item>)}
          </DropdownButton>}
        </div>;
      case "expansion":
        return <div>
          <Expression expression={premise.statement} boundVariableLists={boundVariableLists}/>
          <PremiseChildren>
            {premise.premises.map((p, i) => this.renderPremise(p, [...path, i], boundVariableLists))}
          </PremiseChildren>
        </div>;
      case "given":
      case "simplification":
        return <div>
          <Expression expression={premise.statement} boundVariableLists={boundVariableLists}/>
        </div>;
    }
  }

  fetchOptions = () => {
    this.props.fetchForStep(this.props.path, "premiseOptions")
      .then(response => {
        if (response.ok) {
          return response.json();
        }
      })
      .then(options => this.setState({premiseOptions: options}));
  };

  render() {
    let {step, path, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    const inference = step.inference || step.inferenceApplication.inference;
    const popover = (
      inference && <Popover title={inference.name}>
        <InferenceSummary inference={inference} />
        {step.premises && <>
          <hr/>
          <div><strong>Premises</strong></div>
          {step.premises.map((p, i) => this.renderPremise(p, [i], otherProps.boundVariableLists))}
        </>}
      </Popover>
    );
    return <ProofLine referencedLines={step.referencedLines} popover={popover} onShowPopover={this.fetchOptions} {...otherProps}>
      Then <ProofLineStatement statement={step.statement} boundVariableLists={this.props.boundVariableLists} references={[...additionalReferences, reference]} {...otherProps}/>.
    </ProofLine>;
  }
}

class ScopedVariableStep extends React.Component {
  render() {
    let {step, path, boundVariableLists, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    return <Steps steps={step.substeps} path={path} boundVariableLists={[[step.variableName], ...boundVariableLists]} referencesForLastStep={referencesForLastStep} {...otherProps} />
  }
}

class NamingStep extends React.Component {
  render() {
    const {step, path, boundVariableLists, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine referencedLines={step.finalInferenceApplication.referencedLines} {...otherProps}>
        Let {formatHtml(step.variableName)} be such that <ProofLineStatement statement={step.assumption} boundVariableLists={innerBoundVariableLists} references={[...additionalReferences, reference, reference + "a"]} {...otherProps}/>.
      </ProofLine>
      <Steps steps={step.substeps} path={path} boundVariableLists={innerBoundVariableLists} referencesForLastStep={referencesForLastStep} {...otherProps} />
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
    }).then(this.props.updateTheorem);
  };

  proveWithInference = (inferenceId, substitutions) => {
    this.props.fetchForStep(this.props.path, {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, substitutions})
    }).then(this.props.updateTheorem);
  };

  render() {
    let {step, path, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
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
      <ProofLine step={step} popover={popover} {...otherProps}>Then <ProofLineStatement statement={step.statement} references={[...additionalReferences, reference]} {...otherProps}/>.</ProofLine>
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
    let {steps, className, path, referencesForLastStep, ...otherProps} = this.props;
    let lastIndex = steps.length - 1;
    return <div className={className}>
      {steps.map((step, index) => {
        let newProps = {
          step: step,
          path: [...path, index],
          key: Steps.getKey(step),
          additionalReferences: (index === lastIndex) ? referencesForLastStep || [] : [],
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
    return <HighlightableStatement reference={"p" + this.props.index} highlightedPremises={this.props.highlightedPremises} statement={this.props.premise} boundVariableLists={[]}/>;
  }
}

const TheoremHeader = styled(class TheoremHeader extends React.Component {
  render() {
    const {theorem, className} = this.props;
    return <div className={className}>
      <h3>
        Theorem: {theorem.name}
      </h3>
      <div className="inferenceId">
        {theorem.id}
      </div>
    </div>;
  }
})`
  margin-bottom: 1rem;
`;

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

  updateTheorem = (response) => {
    response.json().then(theoremJSON => {
      const theorem = Parser.parseTheorem(theoremJSON);
      this.setState({theorem: theorem});
    });
  };

  render() {
    const {previousEntry, nextEntry, usages} = this.props;
    const {theorem} = this.state;
    return <div className="inference">
      <div className="navigationLinks">
        {previousEntry && <a className="navigationLink float-left" href={previousEntry.key}>&laquo; {previousEntry.name}</a>}
        {nextEntry && <a className="navigationLink float-right" href={nextEntry.key}>{nextEntry.name} &raquo;</a>}
      </div>

      <TheoremHeader theorem={theorem} />
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
