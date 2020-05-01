import React, {useRef, useState} from "react";
import Form from "react-bootstrap/Form";
import PrettifiedAutosuggest from "./PrettifiedAutosuggest";

export default function PrettifiedAutosuggestOnIndividualWords({value, onChange, getSuggestions, replaceCompletedWord, inputProps, ...otherProps}) {
  const ref = useRef(null);
  const [suggestions, setSuggestions] = useState([]);

  function replaceCompletedWordIfApplicable(text, selectionPosition, requireWhitespace) {
    const initialText = text.substring(0, selectionPosition);
    const finalText = text.substring(selectionPosition);
    const match = initialText.match(new RegExp('(^|\\s)([^\\s]+)' + (requireWhitespace ? '(\\s$)' : '$')));
    const replacedInitialText = (match && replaceCompletedWord) ?
      (initialText.substring(0, match.index + match[1].length) + replaceCompletedWord(match[2]) + initialText.substring(match.index + match[1].length + match[2].length)) :
      initialText;
    const replacedText = replacedInitialText + finalText;
    return [replacedText, replacedInitialText.length];
  }

  function onInnerChange() {
    const [newText, newSelectionStart] = replaceCompletedWordIfApplicable(ref.current.input.value, ref.current.input.selectionStart, true);
    const callback = () => ref.current.input.setSelectionRange(newSelectionStart, newSelectionStart);
    return onChange(newText, callback);
  }

  function onBlur() {
    const [newText, ] = replaceCompletedWordIfApplicable(ref.current.input.value, ref.current.input.value.length, false);
    return onChange(newText);
  }

  function getInputContents() {
    const text = ref.current.input.value;
    const selectionPosition = ref.current.input.selectionStart;
    const initialText = text.substring(0, selectionPosition);
    const finalText = text.substring(selectionPosition);
    return [initialText, finalText];
  }
  function splitLastWord(text) {
    const match = text.match(new RegExp('(^|\\s)([^\\s]*)$'));
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

  function onSuggestionsFetchRequested() {
    const [initialText, ] = getInputContents();
    const [, lastWord] = splitLastWord(initialText);
    setSuggestions(getSuggestions(lastWord));
  }
  function onSuggestionsClearRequested() {
    setSuggestions([]);
  }
  function onSuggestionSelected(event, {suggestion}) {
    replaceLastWord(suggestion);
    event.preventDefault();
    event.stopPropagation();
  }
  function renderInputComponent(inputProps) {
    return <Form.Control {...inputProps}/>
  }

  return <PrettifiedAutosuggest ref={ref}
                                suggestions={suggestions}
                                onSuggestionsFetchRequested={onSuggestionsFetchRequested}
                                onSuggestionsClearRequested={onSuggestionsClearRequested}
                                onSuggestionSelected={onSuggestionSelected}
                                renderInputComponent={renderInputComponent}
                                getSuggestionValue={x => x}
                                {...otherProps}
                                inputProps={{...inputProps, value: value, onChange: onInnerChange, onBlur}}  />;
};
