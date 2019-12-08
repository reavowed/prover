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

export default class extends React.Component {
  constructor(...args) {
    super(...args);
    this.autoSuggestRef = React.createRef();
  }
  componentDidMount() {
    if (this.props.autofocus) {
      this.autoSuggestRef.current.input.focus();
    }
  }

  render() {
    const {getSuggestionValue, suggestions, onSuggestionsFetchRequested, onSuggestionsClearRequested, onSuggestionSelected, value, onValueChange} = this.props;

    function renderSuggestionsContainer ({containerProps, children}) {
      return <div {...containerProps}><DropdownContainer>{children}</DropdownContainer></div>
    }

    return <Autosuggest
      ref={this.autoSuggestRef}
      suggestions={suggestions}
      onSuggestionsFetchRequested={onSuggestionsFetchRequested}
      onSuggestionsClearRequested={onSuggestionsClearRequested}
      shouldRenderSuggestions={() => true}
      getSuggestionValue={getSuggestionValue}
      renderSuggestionsContainer={renderSuggestionsContainer}
      onSuggestionSelected={onSuggestionSelected}
      renderSuggestion={s => <span className="dropdown-item">{getSuggestionValue(s)}</span>}
      inputProps={{value: value, onChange: onValueChange, className:"form-control"}} />
  }
}
