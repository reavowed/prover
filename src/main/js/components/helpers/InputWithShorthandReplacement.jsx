import _ from "lodash";
import React, {useContext, useRef} from "react";
import Form from "react-bootstrap/Form";
import EntryContext from "../EntryContext";

export default function InputWithShorthandReplacement({as, value, onChange, ...otherProps}) {
  const ref = useRef(null);
  const context = useContext(EntryContext);

  function replaceShorthands(text, selectionPosition, requireWhitespace) {
    const initialText = text.substring(0, selectionPosition);
    const finalText = text.substring(selectionPosition);
    const replacedInitialText =_.reduce(_.toPairs(context.definitionShorthands), (text, [valueToReplace, symbol]) => {
      const regex = new RegExp('(?<=^|\\s)' + _.escapeRegExp(valueToReplace) + (requireWhitespace ? '(?=\\s$)' : '$'), 'gim');
      return text.replace(regex, symbol);
    }, initialText);
    const replacedText = replacedInitialText + finalText;
    return [replacedText, replacedInitialText.length];
  }

  function onInnerChange(event) {
    const input = event.target;
    const [newText, newSelectionStart] = replaceShorthands(input.value, input.selectionStart, true);
    const callback = () => ref.current.setSelectionRange(newSelectionStart, newSelectionStart);
    return onChange(newText, callback);
  }

  function onBlur(event) {
    const input = event.target;
    const [newText, ] = replaceShorthands(input.value, input.value.length, false);
    return onChange(newText);
  }

  return <Form.Control ref={ref} as={as} type="text" value={value} onChange={onInnerChange} onBlur={onBlur} {...otherProps}/>
}


