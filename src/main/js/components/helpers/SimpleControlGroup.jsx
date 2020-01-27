import React from "react";
import Form from "react-bootstrap/Form";

const SimpleTextControl = React.forwardRef(function SimpleTextControl({onChange, ...otherProps}, ref) {
  const props = {type: "text", ...otherProps, onChange: (e) => onChange(e.target.value)};
  return <Form.Control ref={ref} {...props} />
});

export default React.forwardRef(function SimpleControlGroup({title, inputType, ...otherProps}, ref) {
  inputType = inputType || SimpleTextControl;
  const input = React.createElement(inputType, {...otherProps, ref});
  return <Form.Group>
    <Form.Label>{title}</Form.Label>
    {input}
  </Form.Group>
});
