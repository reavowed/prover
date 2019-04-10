import React from "react";
import Button from "react-bootstrap/Button";
import Overlay from "react-bootstrap/Overlay";
import Tooltip from "react-bootstrap/Tooltip";
import Modal from "react-bootstrap/Modal";
import Form from "react-bootstrap/Form";
import styled, {css} from "styled-components";
import {HighlightableExpression} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import Popover from "react-bootstrap/Popover";
import {BoundVariableModal} from "../Modals";
import {Parser} from "../../Parser";

export const ProofLine = styled(class ProofLine extends React.Component {
  constructor(...args) {
    super(...args);
    this.attachDivRef = divRef => this.setState({ divRef });
    this.attachSpanRef = spanRef => this.setState({ spanRef });
    this.attachButtonRef = buttonRef => this.setState({ buttonRef });
    this.targetInputRef = React.createRef();
    this.state = {
      isHovered: false,
      shouldShowButtonPopover: false,
      shouldShowSubproofNameModal: false,
      subproofName: '',
      addingTarget: false,
      targetToAdd: ''
    };
  }
  toggleButtonPopover = () => {
    this.setState({shouldShowButtonPopover: !this.state.shouldShowButtonPopover});
  };
  hideButtonPopover = () => {
    this.setState({shouldShowButtonPopover: false});
  };
  onMouseEnter = () => {
    let {highlighting, premiseReferences, path, reference} = this.props;
    if (premiseReferences && highlighting) {
      highlighting.setHighlightedPremises(premiseReferences);
    }
    let conclusionReference = reference || (path && {stepPath: path});
    if (conclusionReference && highlighting) {
      highlighting.setHighlightedConclusion(conclusionReference);
    }
    this.setState({isHovered: true});
  };
  onMouseLeave = () => {
    const {highlighting} = this.props;
    if (highlighting) {
      highlighting.setHighlightedPremises([]);
      highlighting.setHighlightedConclusion(null);
    }
    this.setState({isHovered: false});
  };
  onClick = (e) => {
    if (this.props.onClick) {
      this.props.onClick(e);
    }
  };
  moveUp = (e) => {
    e.stopPropagation();
    this.props.apiService.fetchJsonForStep(this.props.path, "move?direction=up", {method: "POST"})
      .then(this.props.apiService.updateTheorem);
  };
  moveDown = (e) => {
    e.stopPropagation();
    this.props.apiService.fetchJsonForStep(this.props.path, "move?direction=down", {method: "POST"})
      .then(this.props.apiService.updateTheorem);
  };
  moveIntoNext = (e) => {
    e.stopPropagation();
    this.props.apiService.fetchJsonForStep(this.props.path, "moveIntoNext", {method: "POST"})
      .then(this.props.apiService.updateTheorem);
  };
  moveOutOfContainer = (e) => {
    e.stopPropagation();
    this.props.apiService.fetchJsonForStep(this.props.path, "moveOutOfContainer", {method: "POST"})
      .then(this.props.apiService.updateTheorem);
  };
  clearStep = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "clear", {
      method: "POST"
    }).then(this.props.apiService.updateTheorem);
  };
  deleteStep = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "", {
      method: "DELETE"
    }).then(this.props.apiService.updateTheorem);
  };

  showSubproofNameModal = () => {
    this.setState({shouldShowSubproofNameModal: true})
  };
  hideSubproofNameModal = () => {
    this.setState({shouldShowSubproofNameModal: false})
  };
  createSubproof = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "introduceSubproof", {
      method: "POST",
      body: this.state.subproofName
    })
      .then(this.props.apiService.updateTheorem)
      .then(this.hideSubproofNameModal);
  };

  showTargetModal = () => {
    this.setState({addingTarget: true})
  };
  hideTargetModal = () => {
    this.setState({addingTarget: false});
  };
  addTarget = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "target", {
      method: "POST",
      body: this.state.targetToAdd
    })
      .then(this.props.apiService.updateTheorem)
      .then(this.hideTargetModal);
  };

  elide = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "elide", {
      method: "POST"
    }).then(this.props.apiService.updateTheorem);
  };

  render() {
    const {className, children, tooltip, path, buttons} = this.props;

    const subProofNamingModal = <BoundVariableModal show={this.state.shouldShowSubproofNameModal}
                                                    onHide={() => this.hideSubproofNameModal}
                                                    title="Choose sub-proof name"
                                                    value={this.state.subproofName}
                                                    onChange={e => this.setState({subproofName: e.target.value})}
                                                    onSave={this.createSubproof}/>;


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

    const lineElement= <div onMouseEnter={this.onMouseEnter}
                            onMouseLeave={this.onMouseLeave}
                            className={"mb-1 " + className}
                            ref={this.attachDivRef}>
      <FlexRow>
        <span ref={this.attachSpanRef}
              onClick={this.onClick}
              style={this.props.onClick && {cursor: "pointer"}}>
          {children}
        </span>
        <span className="ml-3">
          {buttons}
        </span>
        <FlexRow.Grow/>
        {path && (this.state.isHovered || this.state.shouldShowButtonPopover) && <>
          <Button ref={this.attachButtonRef} onClick={this.toggleButtonPopover} size="sm" className="ml-1"><span className="fas fa-ellipsis-v"/></Button>
          <Overlay target={this.state.buttonRef} show={this.state.shouldShowButtonPopover} onHide={this.hideButtonPopover} rootClose placement="bottom">
            {({show, ...props}) => <Popover {...props}>
              <Button onClick={this.showTargetModal} variant="success" size="sm" className="ml-1">Add target</Button>
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
      {targetModal}
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

ProofLine.Statement = styled(HighlightableExpression)`
  ${ProofLine}:hover & {
    color: blue;
  }
`;
