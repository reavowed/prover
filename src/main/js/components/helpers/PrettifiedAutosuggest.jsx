import React from "react";
import Autosuggest from "react-autosuggest";
import styled from "styled-components";

const DropdownContainer = styled.div`
  .react-autosuggest__suggestions-container--open & {
    background-color: #fff;
    background-clip: padding-box;
    border: 1px solid rgba(0,0,0,.15);
    border-radius: .25rem;
    ul {
      margin: 0;
      padding: 0;
    }
    li {
      list-style-type: none;
      overflow: hidden;
    }
  }
`;
const Suggestion = styled.span`
  ${props => (props.isHighlighted ? "background-color: #007bff; color: #fff" : "")}
`;

export default React.forwardRef(function PrettifiedAutosuggest({renderSuggestion, ...props}, ref) {

  function renderSuggestionsContainer ({containerProps, children}) {
    return <div {...containerProps}><DropdownContainer>{children}</DropdownContainer></div>
  }
  function renderSuggestionInner(suggestion, {isHighlighted}) {
    return <Suggestion className="dropdown-item" isHighlighted={isHighlighted}>{(renderSuggestion || props.getSuggestionValue)(suggestion)}</Suggestion>
  }

  return <Autosuggest
    ref={ref}
    renderSuggestionsContainer={renderSuggestionsContainer}
    renderSuggestion={renderSuggestionInner}
    {...props} />
});
