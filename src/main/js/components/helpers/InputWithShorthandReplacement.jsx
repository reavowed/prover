import _ from "lodash";
import React, {useContext, useRef, useState} from "react";
import Autosuggest from "react-autosuggest";
import Form from "react-bootstrap/Form";
import styled from "styled-components";
import EntryContext from "../EntryContext";

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

export default function InputWithShorthandReplacement({value, onChange, ...otherProps}) {
  const ref = useRef(null);
  const context = useContext(EntryContext);

  function replaceShorthands(text, selectionPosition, requireWhitespace) {
    const initialText = text.substring(0, selectionPosition);
    const finalText = text.substring(selectionPosition);
    const replacedInitialText =_.reduce(_.toPairs(context.definitionShorthands), (text, [valueToReplace, {baseSymbol}]) => {
      const regex = new RegExp('(^|\\s)' + _.escapeRegExp(valueToReplace) + (requireWhitespace ? '(\\s$)' : '$'));
      const match = text.match(regex);
      if (match) {
        return text.substring(0, match.index + match[1].length) + baseSymbol + text.substring(match.index + match[1].length + valueToReplace.length)
      } else {
        return text;
      }
    }, initialText);
    const replacedText = replacedInitialText + finalText;
    return [replacedText, replacedInitialText.length];
  }

  function onInnerChange() {
    const [newText, newSelectionStart] = replaceShorthands(ref.current.input.value, ref.current.input.selectionStart, true);
    const callback = () => ref.current.input.setSelectionRange(newSelectionStart, newSelectionStart);
    return onChange(newText, callback);
  }

  function onBlur() {
    const [newText, ] = replaceShorthands(ref.current.input.value, ref.current.input.value.length, false);
    return onChange(newText);
  }

  const [suggestions, setSuggestions] = useState([]);

  function getInputContents() {
    const text = ref.current.input.value;
    const selectionPosition = ref.current.input.selectionStart;
    const initialText = text.substring(0, selectionPosition);
    const finalText = text.substring(selectionPosition);
    return [initialText, finalText];
  }
  function splitLastWord(text) {
    const match = text.match(new RegExp('(^|\\s)(\\w+)$'));
    if (match) {
      return [text.substring(0, match.index + match[1].length), match[2]];
    } else {
      return [text, null];
    }
  }
  function replaceInputInitialText(f) {
    const [initialText, finalText] = getInputContents();
    const newInitialText = f(initialText);
    const newText = newInitialText + finalText;
    const newSelectionStart = newInitialText.length;
    const callback = () => ref.current.input.setSelectionRange(newSelectionStart, newSelectionStart);
    return onChange(newText, callback);
  }
  function replaceLastWord(newWord) {
    replaceInputInitialText(initialText => {
      const [preceding,] = splitLastWord(initialText);
      return preceding + newWord;
    });
  }
  function matchWords(words, searchText) {
    if (!searchText.length) {
      return true;
    }
    if (!words.length) {
      return false;
    }
    const [firstWord, ...remainingWords] = words;
    if (!firstWord.length) {
      return matchWords(remainingWords, searchText);
    }
    if (firstWord[0].toUpperCase() === searchText[0].toUpperCase()) {
      return matchWords([firstWord.substring(1), ...remainingWords], searchText.substring(1)) ||
        matchWords(remainingWords, searchText.substring(1));
    } else {
      return matchWords(remainingWords, searchText);
    }
  }

  function isMatch(key, searchText) {
    const keyWords = key.split(/(?=[A-Z])/);
    return matchWords(keyWords, searchText);
  }

  function onSuggestionsFetchRequested() {
    const [initialText, ] = getInputContents();
    const [, lastWord] = splitLastWord(initialText);
    if (lastWord && lastWord.length > 1) {
      const matchingDefinitions = _.chain(context.definitions)
        .filter((value, key) => isMatch(key, lastWord))
        .map((value) => value.symbol)
        .value();
      const matchingShorthands = _.chain(context.definitionShorthands)
        .filter((value, key) => isMatch(key, lastWord))
        .map(({baseSymbol}) => baseSymbol)
        .value();
      const matchingTypes = _.chain(context.typeDefinitions)
        .flatMap((value, key) => [key, ..._.values(value.properties).map(p => p.symbol)])
        .filter(key => isMatch(key, lastWord))
        .value();
      const matchingValues = _.chain([...matchingDefinitions, ...matchingShorthands, ...matchingTypes]).uniq().sortBy().value().slice(0, 10);
      setSuggestions(matchingValues);
    } else {
      setSuggestions([]);
    }
  }
  function onSuggestionsClearRequested() {
    setSuggestions([]);
  }
  function onSuggestionSelected(event, {suggestion}) {
    replaceLastWord(suggestion);
    event.preventDefault();
    event.stopPropagation();
  }

  function renderSuggestionsContainer ({containerProps, children}) {
    return <div {...containerProps}><DropdownContainer>{children}</DropdownContainer></div>
  }
  function renderSuggestion(suggestion, {isHighlighted}) {
    return <Suggestion className="dropdown-item" isHighlighted={isHighlighted}>{suggestion}</Suggestion>
  }
  function renderInputComponent(inputProps) {
    return <Form.Control {...inputProps}/>
  }

  return <Autosuggest ref={ref}
                      suggestions={suggestions}
                      onSuggestionsFetchRequested={onSuggestionsFetchRequested}
                      onSuggestionsClearRequested={onSuggestionsClearRequested}
                      onSuggestionSelected={onSuggestionSelected}
                      renderSuggestionsContainer={renderSuggestionsContainer}
                      renderSuggestion={renderSuggestion}
                      renderInputComponent={renderInputComponent}
                      getSuggestionValue={_ => value}
                      inputProps={{...otherProps, value: value, onChange: onInnerChange, onBlur}}  />;
}


