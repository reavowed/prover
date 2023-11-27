import styled from "styled-components";

export type ExpressionHighlightingProps = {
  isPremise: boolean
  isConclusion: boolean
  isClickable: boolean
  onClick?: React.MouseEventHandler<HTMLSpanElement>
}

export default styled.span`
    color: ${(props: ExpressionHighlightingProps) => props.isPremise ? "red" : props.isConclusion ? "blue" : null};
    cursor: ${(props: ExpressionHighlightingProps) => props.isClickable && "pointer"};
`;
