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

export default class InferenceAutosuggest extends React.Component {
  constructor(...args) {
    super(...args);
    this.autoSuggestRef = React.createRef();
    this.state = {
      autosuggestValue: "",
      valueCurrentlyBeingFetched: null,
      lastFetchResult: null,
      suggestions: []
    }
  }
  componentDidMount() {
    if (this.props.autofocus) {
      this.autoSuggestRef.current.input.focus();
    }
  }

  onAutosuggestChange = (event, { newValue }) => {
    this.setState({autosuggestValue: newValue});
  };
  onSuggestionsFetchRequested = ({value}) => {
    if (this.state.lastFetchResult && this.state.lastFetchResult.value === value) {
      this.setState({suggestions: this.state.lastFetchResult.suggestions})
    } else if (value !== this.state.valueCurrentlyBeingFetched) {
      this.setStatePromise({valueCurrentlyBeingFetched: value})
        .then(() => this.props.fetchSuggestions(value))
        .then(suggestions => {
          if (this.state.autosuggestValue === value) {
            this.setState({
              suggestions,
              valueCurrentlyBeingFetched: null,
              lastFetchResult: {value, suggestions}
            })
          }
        });
    }
  };
  onSuggestionsClearRequested = () => {
    this.setState({suggestions: []});
  };
  onSuggestionSelected = (event, {suggestion}) => {
    this.props.setSelectedSuggestion(suggestion);
  };

  render() {
    const {getSuggestionValue, renderSuggestion, readOnly} = this.props;

    function renderSuggestionsContainer ({containerProps, children}) {
      return <div {...containerProps}><DropdownContainer>{children}</DropdownContainer></div>
    }

    return <Autosuggest
      ref={this.autoSuggestRef}
      suggestions={this.state.suggestions}
      onSuggestionsFetchRequested={this.onSuggestionsFetchRequested}
      onSuggestionsClearRequested={this.onSuggestionsClearRequested}
      onSuggestionSelected={this.onSuggestionSelected}
      shouldRenderSuggestions={() => true}
      getSuggestionValue={getSuggestionValue}
      renderSuggestionsContainer={renderSuggestionsContainer}
      renderSuggestion={s => <span className="dropdown-item">{(renderSuggestion || getSuggestionValue)(s)}</span>}
      inputProps={{value: this.state.autosuggestValue, onChange: this.onAutosuggestChange, className:"form-control", readOnly}} />
  }
}
