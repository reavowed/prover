import React from "react";
import styled from "styled-components";
import _ from "lodash";

export class Expression extends React.Component {
  render() {
    return <span className={this.props.className} dangerouslySetInnerHTML={{ __html: this.props.expression.toHtml([], false)}} />;
  }
}

export const HighlightableExpression = styled(Expression)`
  color: ${props => _.some(props.highlightedPremises, p => p.lineReference === props.reference) && "red"};
`;
