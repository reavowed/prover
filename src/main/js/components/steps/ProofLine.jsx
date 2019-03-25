import React from "react";
import Button from "react-bootstrap/Button";
import Overlay from "react-bootstrap/Overlay";
import styled, {css} from "styled-components";
import {HighlightableStatement} from "../Expression";

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
    const {setHighlightedPremises, referencedLines} = this.props;
    if (referencedLines && setHighlightedPremises) {
      setHighlightedPremises(referencedLines);
    }
    this.setState({isHovered: true});
  };
  onMouseLeave = () => {
    const {setHighlightedPremises} = this.props;
    if (setHighlightedPremises) {
      setHighlightedPremises([]);
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
    const lineElement= <div onMouseEnter={this.onMouseEnter} onMouseLeave={this.onMouseLeave} onClick={this.showPopover} className={className} ref={this.attachRef}>
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
  padding-bottom: 5px;
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

ProofLine.Statement = styled(HighlightableStatement)`
  ${ProofLine}:hover & {
    color: blue;
  }
`;