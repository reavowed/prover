import React from "react";
import Form from "react-bootstrap/Form";
import SimpleTextControl from "./SimpleTextControl";

export default React.forwardRef(function SimpleControlGroup({title, inputType, ...otherProps}, ref) {
  inputType = inputType || SimpleTextControl;
  const input = React.createElement(inputType, {...otherProps, ref});
  return <Form.Group>
    <Form.Label>{title}</Form.Label>
    {input}
  </Form.Group>
});
