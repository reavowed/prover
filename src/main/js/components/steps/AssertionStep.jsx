import React from "react";
import Button from "react-bootstrap/Button";
import Dropdown from "react-bootstrap/Dropdown";
import DropdownButton from "react-bootstrap/DropdownButton";
import Popover from "react-bootstrap/Popover";
import styled from "styled-components";
import {Parser} from "../../Parser";
import {Expression, HighlightableExpression} from "../Expression";
import {FlexRow} from "../FlexRow";
import {InferenceSummary} from "../InferenceSummary";
import {BoundVariableModal} from "../Modals";
import {DeleteStepButton} from "./DeleteStepButton";
import {ProofLine} from "./ProofLine";

const PremiseChildren = styled.div`
  margin-left: 20px;
`;
const EditableBoundVariable = styled.span`
  &:hover {
    color: red;
    cursor: pointer;
  }
`;

export class AssertionStep extends React.Component {
  constructor(...args) {
    super(...args);
    this.state = {
      premiseOptions: {},
      boundVariableLocation: null,
      boundVariableName: ''
    };
  }

  fetchOptions = () => {
    this.props.fetchForStep(this.props.path, "premiseOptions")
      .then(response => {
        if (response.ok) {
          return response.json();
        }
      })
      .then(options => {
        _.each(options, option => _.each(option.quick, t => t.target = Parser.parseExpression(t.target)));
        this.setState({premiseOptions: options})
      });
  };

  showBoundVariableModal = () => {
    return this.state.boundVariableLocation != null;
  };
  hideBoundVariableModal = () => {
    setTimeout(() => this.setState({boundVariableLocation: null}), 0);
  };
  updateBoundVariableName = (event) => {
    this.setState({boundVariableName: event.target.value});
  };
  saveBoundVariable = () => {
    const {boundVariableIndex, boundVariablePath, premisePath} = this.state.boundVariableLocation;
    this.props.fetchForStep(this.props.path, `premises/${premisePath.join(".")}/statement/${boundVariablePath.join(".")}/boundVariables/${boundVariableIndex}/`, {
      method: "PUT",
      body: this.state.boundVariableName
    })
      .then(this.props.updateTheorem)
      .then(this.hideBoundVariableModal);
  };

  render() {
    let {step, path, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    const boundVariableModal = <BoundVariableModal show={this.showBoundVariableModal()}
                                                   onHide={this.hideBoundVariableModal}
                                                   title="Edit bound variable"
                                                   value={this.state.boundVariableName}
                                                   onChange={this.updateBoundVariableName}
                                                   onSave={this.saveBoundVariable}/>;

    return <>
      <ProofLine premiseReferences={step.referencedLines}
                 path={path}
                 popover={<AssertionStep.Popover step={step} path={path} premiseOptions={this.state.premiseOptions} {...otherProps}/>}
                 onShowPopover={this.fetchOptions}
                 blockHide={this.showBoundVariableModal()}
                 incomplete={_.some(step.premises, "incomplete")}
                 {...otherProps}
      >
        Then <HighlightableExpression statement={step.statement}
                                      boundVariableLists={this.props.boundVariableLists}
                                      references={[...additionalReferences, reference]}
                                      {...otherProps}
      />.
      </ProofLine>
      {boundVariableModal}
    </>;
  }
}

AssertionStep.Popover = class extends React.Component {
  applyExpansion = (premisePath, expansionId) => {
    this.props.fetchForStep(this.props.path, `premises/${premisePath.join(".")}/rearrangement`, {
      method: "POST",
      body: expansionId
    }).then(this.props.updateTheorem);
  };
  applyQuick = (premisePath, inferenceId, target) => {
    this.props.fetchForStep(this.props.path, `premises/${premisePath.join(".")}/quick`, {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({inferenceId, target: target.serialize()})
    }).then(this.props.updateTheorem);
  };

  addTarget = (premisePath) => {
    this.props.fetchForStep(this.props.path, `premises/${premisePath.join(".")}/target`, {
      method: "POST"
    }).then(this.props.updateTheorem);
  };

  renderPremise(premise, path, boundVariableLists) {
    switch (premise.type) {
      case "pending":
        const options = _.find(this.props.premiseOptions, option => _.isEqual(option.path, path)) || {};
        const wrapEditableBoundVariable = (boundVariableContent, boundVariableName, boundVariableIndex, boundVariablePath) => {
          const startEditingBoundVariable = () => {
            this.setState({
              boundVariableLocation: {boundVariableIndex, boundVariablePath, premisePath: path},
              boundVariableName: boundVariableName
            });
          };
          return <EditableBoundVariable onClick={startEditingBoundVariable}>{boundVariableContent}</EditableBoundVariable>;
        };

        return <FlexRow>
          <FlexRow.Grow><Expression expression={premise.statement} boundVariableLists={boundVariableLists} wrapBoundVariable={wrapEditableBoundVariable}/></FlexRow.Grow>
          <Button size="sm" className="ml-1" onClick={() => this.addTarget(path)}>Target</Button>
          {options.expansions && options.expansions.length > 0 && <DropdownButton title="Expansions" size="sm" className="ml-1">
            {options.expansions.map(e => <Dropdown.Item key={e.id} onClick={() => this.applyExpansion(path, e.id)}>{e.name}</Dropdown.Item>)}
          </DropdownButton>}
          {options.quick && options.quick.length > 0 && <DropdownButton title="Quick" size="sm" className="ml-1">
            {options.quick.map(e => {
              const content = _.countBy(options.quick, "inference.id")[e.inference.id] > 1 ?
                <>{e.inference.name} - <Expression expression={e.target} boundVariableLists={boundVariableLists}/></> :
                e.inference.name;
              return <Dropdown.Item key={e.id + " " + e.target.serialize()} onClick={() => this.applyQuick(path, e.inference.id, e.target)}>{content}</Dropdown.Item>
            })}
          </DropdownButton>}
        </FlexRow>;
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

  render() {
    const {step, path, boundVariableLists, innerRef} = this.props;
    const inference = step.inference || step.inferenceApplication.inference;
    return <Popover ref={innerRef} title={<FlexRow><FlexRow.Grow><a href={inference.key.url}>{inference.name}</a></FlexRow.Grow>{path && <DeleteStepButton path={path} {...this.props}/>}</FlexRow>}  {...this.props}>
        <InferenceSummary inference={inference} />
        {step.premises && <>
          <hr/>
          <div><strong>Premises</strong></div>
          {step.premises.map((p, i) => <React.Fragment key={p.statement.serialize()}>{this.renderPremise(p, [i], boundVariableLists)}</React.Fragment>)}
        </>}
      </Popover>;
  }Ta
}
