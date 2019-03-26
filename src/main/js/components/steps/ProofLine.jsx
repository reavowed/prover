import React from "react";
import Button from "react-bootstrap/Button";
import Overlay from "react-bootstrap/Overlay";
import styled, {css} from "styled-components";
import {HighlightableExpression} from "../Expression";

export const ProofLine = styled(class ProofLine extends React.Component {
  constructor(...args) {
    super(...args);
    this.attachRef = target => this.setState({ target });
    this.state = {
      showPopover: false,
      isHovered: false
    };
  }
  showPopover = () => {
    this.setState({showPopover: true})
  };
  hidePopover = () => {
    if (!this.props.blockHide) {
      this.setState({showPopover: false})
    }
  };
  onMouseEnter = () => {
    let {highlighting, premiseReferences, path} = this.props;
    if (premiseReferences && highlighting) {
      highlighting.setHighlightedPremises(premiseReferences);
    }
    if (path && highlighting) {
      highlighting.setHighlightedConclusion(path.join("."));
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
  moveUp = () => {
    this.props.fetchForStep(this.props.path, "move?direction=up", {
      method: "POST"
    }).then(this.props.updateTheorem);
  };
  moveDown = () => {
    this.props.fetchForStep(this.props.path, "move?direction=down", {
      method: "POST"
    }).then(this.props.updateTheorem);
  };
  render() {
    const {className, children, popover, onShowPopover, path} = this.props;
    const lineElement= <div onMouseEnter={this.onMouseEnter} onMouseLeave={this.onMouseLeave} onClick={this.showPopover} className={"mb-1 " + className} ref={this.attachRef}>
      {children}
      {path && this.state.isHovered && <span className="float-right">
        <Button onClick={this.moveUp} size="sm" className="ml-1"><span className="fas fa-arrow-up"/></Button>
        <Button onClick={this.moveDown} size="sm" className="ml-1"><span className="fas fa-arrow-down"/></Button>
      </span>}
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
