import styled from "styled-components";

const FlexRow = styled.div`
  display: flex;
`;

const FlexRowGrow = styled.div`
  flex-grow: 1;
`;

export default Object.assign(FlexRow, {
  Grow: FlexRowGrow
})
