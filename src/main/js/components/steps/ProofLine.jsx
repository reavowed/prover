import React from "react";
import Button from "react-bootstrap/Button";
import Overlay from "react-bootstrap/Overlay";
import Tooltip from "react-bootstrap/Tooltip";
import styled, {css} from "styled-components";
import {HighlightableExpression} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {DeleteStepButton} from "./DeleteStepButton";

export const ProofLine = styled(class ProofLine extends React.Component {
  constructor(...args) {
    super(...args);
    this.attachDivRef = divRef => this.setState({ divRef });
    this.attachSpanRef = spanRef => this.setState({ spanRef });
    this.state = {
      isHovered: false
    };
  }
  onMouseEnter = () => {
    let {highlighting, premiseReferences, path, reference} = this.props;
    reference = reference || (path && path.join("."));
    if (premiseReferences && highlighting) {
      highlighting.setHighlightedPremises(premiseReferences);
    }
    if (reference && highlighting) {
      highlighting.setHighlightedConclusion(reference);
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
  render() {
    const {className, children, tooltip, path, apiService, buttons} = this.props;
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
        {path && this.state.isHovered && <>
          <DeleteStepButton path={path} apiService={apiService}/>
          <Button onClick={this.moveUp} size="sm" className="ml-1"><span className="fas fa-arrow-up"/></Button>
          <Button onClick={this.moveDown} size="sm" className="ml-1"><span className="fas fa-arrow-down"/></Button>
        </>}
      </FlexRow>
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
