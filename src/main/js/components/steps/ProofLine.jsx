import React from "react";
import Button from "react-bootstrap/Button";
import Overlay from "react-bootstrap/Overlay";
import Popover from "react-bootstrap/Popover";
import Tooltip from "react-bootstrap/Tooltip";
import {connect} from "react-redux";
import styled, {css} from "styled-components";
import {StepReference} from "../../models/Step";
import {HighlightableExpression} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {InlineTextEditor} from "../helpers/InlineTextEditor";
import {BoundVariableModal} from "../Modals";
import ProofContext from "../theorem/ProofContext";
import {FetchJsonForStepAndUpdate, SetHighlightedConclusion, SetHighlightedPremises} from "../theorem/TheoremStore";

const ProofLine = connect()(styled(class ProofLine extends React.Component {
  static contextType = ProofContext;
  constructor(...args) {
    super(...args);
    this.attachSpanRef = spanRef => this.setState({ spanRef });
    this.attachButtonRef = buttonRef => this.setState({ buttonRef });
    this.state = {
      isHovered: false,
      shouldShowButtonPopover: false,
      shouldShowSubproofNameModal: false,
      subproofName: '',
      addingTarget: false,
      targetToAdd: '',
      findInferenceModalCallbacks: null
    };
  }
  toggleButtonPopover = () => {
    this.setState({shouldShowButtonPopover: !this.state.shouldShowButtonPopover});
  };
  hideButtonPopover = () => {
    this.setState({shouldShowButtonPopover: false});
  };
  onMouseEnter = () => {
    let {premiseReferences, path, suffix} = this.props;
    if (premiseReferences) {
      this.props.dispatch(SetHighlightedPremises(premiseReferences));
    }
    let conclusionReference = path && new StepReference(path, suffix || null);
    if (conclusionReference) {
      this.props.dispatch(SetHighlightedConclusion(conclusionReference));
    }
    this.setState({isHovered: true});
  };
  onMouseLeave = () => {
    this.props.dispatch(SetHighlightedPremises([]));
    this.props.dispatch(SetHighlightedConclusion(null));
    this.setState({isHovered: false});
  };
  onClick = (e) => {
    if (this.props.onClick) {
      this.props.onClick(e);
    }
  };
  moveUp = (e) => {
    e.stopPropagation();
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "move?direction=up", {method: "POST"}));
  };
  moveDown = (e) => {
    e.stopPropagation();
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "move?direction=down", {method: "POST"}));
  };
  moveIntoNext = (e) => {
    e.stopPropagation();
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "moveIntoNext", {method: "POST"}));
  };
  moveOutOfContainer = (e) => {
    e.stopPropagation();
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "moveOutOfContainer", {method: "POST"}));
  };
  clearStep = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "clear", {method: "POST"}));
  };
  deleteStep = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "", {method: "DELETE"}));
  };

  showSubproofNameModal = () => {
    this.setState({shouldShowSubproofNameModal: true})
  };
  hideSubproofNameModal = () => {
    this.setState({shouldShowSubproofNameModal: false})
  };
  createSubproof = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "introduceSubproof", {
      method: "POST",
      body: this.state.subproofName
    }))
      .then(this.hideSubproofNameModal);
  };

  elide = () => {
    this.props.dispatch(FetchJsonForStepAndUpdate(this.context.proofIndex, this.props.path, "elide", {method: "POST"}));
  };


  render() {
    const {className, children, tooltip, path, buttons} = this.props;

    const subProofNamingModal = <BoundVariableModal show={this.state.shouldShowSubproofNameModal}
                                                    onHide={() => this.hideSubproofNameModal}
                                                    title="Choose sub-proof name"
                                                    value={this.state.subproofName}
                                                    onChange={e => this.setState({subproofName: e.target.value})}
                                                    onSave={this.createSubproof}/>;

    const lineElement= <div onMouseEnter={this.onMouseEnter}
                            onMouseLeave={this.onMouseLeave}
                            className={"mb-1 " + className}>
      <FlexRow>
        <span ref={this.attachSpanRef}
              onClick={this.onClick}
              style={this.props.onClick && {cursor: "pointer"}}>
          {_.isFunction(children) ? children(this.state.isHovered) : children}
        </span>
        <span className="ml-3">
          {buttons}
        </span>
        <FlexRow.Grow/>
        {path && (this.state.isHovered || this.state.shouldShowButtonPopover) && <>
          <Button ref={this.attachButtonRef} onClick={this.toggleButtonPopover} size="sm" className="ml-1 mb-n2"><span className="fas fa-ellipsis-v"/></Button>
          <Overlay target={this.state.buttonRef} show={this.state.shouldShowButtonPopover} onHide={this.hideButtonPopover} rootClose placement="bottom">
            {({show, ...props}) => <Popover {...props}>
              <Button onClick={this.showSubproofNameModal} variant="success" size="sm" className="ml-1">To subproof</Button>
              <Button onClick={this.elide} variant="success" size="sm" className="ml-1">Elide</Button>
              <Button onClick={this.clearStep} variant="danger" size="sm" className="ml-1"><span className="fas fa-redo"/></Button>
              <Button onClick={this.deleteStep} variant="danger" size="sm" className="ml-1"><span className="fas fa-trash"/></Button>
              <Button onClick={this.moveOutOfContainer} size="sm" className="ml-1"><span className="fas fa-level-up-alt"/></Button>
              <Button onClick={this.moveUp} size="sm" className="ml-1"><span className="fas fa-arrow-up"/></Button>
              <Button onClick={this.moveDown} size="sm" className="ml-1"><span className="fas fa-arrow-down"/></Button>
              <Button onClick={this.moveIntoNext} size="sm" className="ml-1"><span className="fas fa-level-down-alt"/></Button>
            </Popover>}
          </Overlay>
        </>}
      </FlexRow>
      {subProofNamingModal}
    </div>;

    if (tooltip) {
      return <>
        {lineElement}
        <Overlay target={this.state.spanRef} show={this.state.isHovered} placement="right">
          {({show, ...props}) => <Tooltip {...props}>{tooltip}</Tooltip>}
        </Overlay>
      </>
    } else {
      return lineElement;
    }
  }
})`
  position: relative;
  ${props => props.incomplete && css`
    &::before {
      content: "?";
      color: red;
      font-weight: bold;
      position: absolute;
      left: -10px;
    }
  `}
`);

ProofLine.SingleStatementWithPrefixContent = connect()(class SingleStatementWithPrefixContent extends React.Component {
  static contextType = ProofContext;

  render() {
    const {editableBoundVariable, prefix, statement, path, suffix, additionalReferences, boundVariableLists, dispatch} = this.props;
    const context = this.context;
    const wrapEditableBoundVariable = (name, index, boundVariablePath) => {
      const callback = (newName) => {
        return dispatch(FetchJsonForStepAndUpdate(context.proofIndex, path, `boundVariables/${boundVariablePath.join(".")}/${index}/`, {
          method: "PUT",
          body: newName
        }))
      };
      return <InlineTextEditor text={name} callback={callback} />;
    };
    return <span onContextMenu={() => navigator.clipboard.writeText(statement.serializeNicely(boundVariableLists))}>
      {prefix}
      {statement && <>
        {' '}
        <HighlightableExpression expression={statement}
                                 boundVariableLists={boundVariableLists}
                                 references={[new StepReference(path, suffix)]}
                                 additionalReferences={additionalReferences || []}
                                 wrapBoundVariable={editableBoundVariable && wrapEditableBoundVariable}/>
       </>}
      {'.'}
    </span>
  }
});

ProofLine.SingleStatementWithPrefix = class SingleStatementWithPrefix extends React.Component {
  render() {
    return <ProofLine {...this.props}>
      <ProofLine.SingleStatementWithPrefixContent {...this.props}/>
    </ProofLine>
  }
};

export default ProofLine;
