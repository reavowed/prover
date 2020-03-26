import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import Overlay from "react-bootstrap/Overlay";
import Popover from "react-bootstrap/Popover";
import Tooltip from "react-bootstrap/Tooltip";
import styled, {css} from "styled-components";
import {StepReference} from "../../../../../models/Step";
import DraggableList from "../../../../DraggableList";
import {HighlightableExpression} from "../../../../ExpressionComponent";
import {FlexRow} from "../../../../FlexRow";
import {InlineTextEditor} from "../../../../helpers/InlineTextEditor";
import {BoundVariableModal} from "../../../../Modals";
import ProofContext from "../../ProofContext";

const ProofLineBase = styled(class ProofLine extends React.Component {
  static contextType = ProofContext;
  constructor(...args) {
    super(...args);
    this.attachSpanRef = spanRef => this.setState({ spanRef });
    this.attachButtonRef = buttonRef => this.setState({ buttonRef });
    this.state = {
      isHovered: false,
      isFocused: false,
      shouldShowSubproofNameModal: false,
      subproofName: '',
      addingTarget: false,
      targetToAdd: '',
      findInferenceModalCallbacks: null
    };
  }
  onMouseEnter = () => {
    let {premiseReferences, path, suffix} = this.props;
    this.context.setHighlighting(
      premiseReferences || [],
      path && new StepReference(path, suffix || null)
    );
    this.setState({isHovered: true});
  };
  onMouseLeave = () => {
    this.context.setHighlighting([], null);
    this.setState({isHovered: false});
  };
  onClick = (e) => {
    if (this.props.onClick) {
      this.props.onClick(e);
    }
  };
  onKeyDown = (event) => {
    if (event.target instanceof HTMLTextAreaElement || event.target instanceof HTMLInputElement) {
      return;
    }
    if (event.shiftKey || event.ctrlKey || event.altKey) {
      return;
    }
    if (event.key === "d") {
      this.deleteStep();
    } else if (event.key === "r") {
      this.clearStep();
    } else if (event.key === "e") {
      this.elide();
    } else if (this.props.onKeyDown) {
      this.props.onKeyDown(event);
    }
  };

  clearStep = () => {
    this.context.fetchJsonForStepAndReplace(this.props.path, "clear", {method: "POST"})
      .then(() => this.context.callOnStep(this.props.path, "startProving"));
  };
  deleteStep = () => {
    this.context.fetchJsonForStepAndReplace(this.props.path, "", {method: "DELETE"});
  };

  showSubproofNameModal = () => {
    this.setState({shouldShowSubproofNameModal: true})
  };
  hideSubproofNameModal = () => {
    this.setState({shouldShowSubproofNameModal: false})
  };
  createSubproof = () => {
    this.context.fetchJsonForStepAndReplace(this.props.path, "introduceSubproof", {
      method: "POST",
      body: this.state.subproofName
    }).then(this.hideSubproofNameModal);
  };

  elide = () => {
    this.context.fetchJsonForStepAndReplace(this.props.path, "elide", {method: "POST"});
  };


  render() {
    const {className, children, tooltip, path, buttons, containerRef} = this.props;

    const subProofNamingModal = <BoundVariableModal show={this.state.shouldShowSubproofNameModal}
                                                    onHide={this.hideSubproofNameModal}
                                                    title="Choose sub-proof name"
                                                    value={this.state.subproofName}
                                                    onChange={e => this.setState({subproofName: e.target.value})}
                                                    onSave={this.createSubproof}/>;

    const lineElement= <div onMouseEnter={this.onMouseEnter}
                            onMouseOver={this.onMouseEnter}
                            onMouseLeave={this.onMouseLeave}
                            onFocus={() => this.setState({isFocused: true})}
                            onBlur={() => this.setState({isFocused: false})}
                            onKeyDown={this.onKeyDown}
                            tabIndex={0}
                            ref={containerRef}
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
        <span className="mb-n2" ref={this.attachButtonRef}>
          {this.state.isHovered && <DraggableList.DragHandle as="span" key="handle">
            <Button as="span" size="sm" className="ml-1"><span className="fas fa-arrows-alt-v"/></Button>
          </DraggableList.DragHandle>}
          {path && (this.state.isHovered || this.state.isFocused) && <>
            <Overlay target={this.state.buttonRef} show={this.state.isFocused} placement="bottom">
              {({show, ...props}) => <Popover {...props}>
                <Button onClick={this.showSubproofNameModal} variant="success" size="sm" className="ml-1">To subproof</Button>
                <Button onClick={this.elide} variant="success" size="sm" className="ml-1">Elide</Button>
                <Button onClick={this.clearStep} variant="danger" size="sm" className="ml-1"><span className="fas fa-redo"/></Button>
                <Button onClick={this.deleteStep} variant="danger" size="sm" className="ml-1"><span className="fas fa-trash"/></Button>
              </Popover>}
            </Overlay>
          </>}
        </span>
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
`;

const ProofLine = React.forwardRef((props, ref) => <ProofLineBase {...props} containerRef={ref} />)

ProofLine.SingleStatementWithPrefixContent = function({editableBoundVariable, prefix, statement, path, suffix, additionalReferences}) {
  const context = useContext(ProofContext);
  const wrapEditableBoundVariable = (name, index, boundVariablePath) => {
    const callback = (newName) => {
      return context.fetchJsonForStepAndReplace(path, `boundVariables/${boundVariablePath.join(".")}/${index}/`, {
        method: "PUT",
        body: newName
      });
    };
    return <InlineTextEditor text={name} callback={callback} />;
  };
  return <>
    {prefix}
    {' '}
    {statement ?
      <HighlightableExpression expression={statement}
                              references={[new StepReference(path, suffix)]}
                              additionalReferences={additionalReferences || []}
                              wrapBoundVariable={editableBoundVariable && wrapEditableBoundVariable}/> :
      "???"}
    {'.'}
  </>
};

ProofLine.SingleStatementWithPrefix = class SingleStatementWithPrefix extends React.Component {
  render() {
    return <ProofLine {...this.props}>
      <ProofLine.SingleStatementWithPrefixContent {...this.props}/>
    </ProofLine>
  }
};

export default ProofLine;
