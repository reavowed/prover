import React, {useContext} from "react";
import Button from "react-bootstrap/Button";
import styled, {css} from "styled-components";
import FlexRow from "../../FlexRow";
import ChapterContext from "./ChapterContext";
import DeleteEntryButton from "./DeleteEntryButton";

const ChapterEntryContainer = styled.div`
  margin: 10px 0;
  padding: 5px 10px;
  border: 1px solid rgba(0,0,0,0.5);
  border-radius: 5px;
`;

const ChapterEntryTitle = styled.a`
  color: black;
  font-weight: bold;
  
  &:hover {
    color: black;
  }
  
  ${props => props.incomplete && css`
    &::before {
      content: "?";
      color: red;
      font-weight: bold;
      margin-right: 0.25em;
    }
  `}
`;

export default function ChapterEntryWrapper({title, url, buttons, children, incomplete}) {
  const context = useContext(ChapterContext);
  return <ChapterEntryContainer>
    <FlexRow>
      <FlexRow.Grow><ChapterEntryTitle href={url} incomplete={incomplete}>{title}</ChapterEntryTitle></FlexRow.Grow>
      {buttons}
      {context.editing && <DeleteEntryButton url={url}/>}
    </FlexRow>
    {children}
  </ChapterEntryContainer>
};
